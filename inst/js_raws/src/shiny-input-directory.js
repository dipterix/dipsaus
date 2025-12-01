/**
 * Directory Input Binding for Shiny
 * Custom implementation that handles directory uploads with actual file content
 */

export function register_directoryInput(Shiny, debug = false) {
  if(!Shiny || !Shiny.inputBindings) {
    if(debug) {
      console.warn("Shiny not found, cannot register directoryInput binding");
    }
    return;
  }

  // Create a new input binding for directory inputs with higher priority
  const directoryInputBinding = new Shiny.InputBinding();

  $.extend(directoryInputBinding, {
    find: function(scope) {
      // Find our specific directory input wrapper elements
      return $(scope).find('.dipsaus-fancy-directory-input');
    },

    initialize: function(el) {
      const $el = $(el);
      const $fileInput = $el.find('input[type="file"]');
      const $textInput = $el.find('input[type="text"]');
      const $inputGroup = $el.find('.input-group');
      const binding = this;
      
      // Mark file input to prevent Shiny's default file input binding from handling it
      $fileInput.addClass('shiny-bound-input');
      
      // Attributes should already be set by R, but ensure they exist
      $fileInput.attr('webkitdirectory', '');
      $fileInput.attr('directory', '');
      $fileInput.attr('multiple', '');
      
      // Store reference to binding for file upload
      $fileInput.data('directoryInputBinding', this);
      
      // Update text input when files are selected
      $fileInput.on('change', function() {
        const files = this.files;
        if(files && files.length > 0) {
          // Get directory name from first file
          const firstPath = files[0].webkitRelativePath || files[0].name;
          const dirName = firstPath.split('/')[0] || 'Selected directory';
          $textInput.val(dirName + ' (' + files.length + ' files)');
        } else {
          $textInput.val('');
        }
      });
      
      // Add drag and drop functionality
      $inputGroup.on('dragover', function(e) {
        e.preventDefault();
        e.stopPropagation();
        $el.addClass('dragging');
      });
      
      $inputGroup.on('dragleave', function(e) {
        e.preventDefault();
        e.stopPropagation();
        $el.removeClass('dragging');
      });
      
      $inputGroup.on('drop', async function(e) {
        e.preventDefault();
        e.stopPropagation();
        $el.removeClass('dragging');
        
        const items = e.originalEvent.dataTransfer.items;
        if(items && items.length > 0) {
          // Check if we can access directory structure
          const item = items[0];
          if(item.webkitGetAsEntry) {
            const entry = item.webkitGetAsEntry();
            if(entry && entry.isDirectory) {
              // Collect all files from the dropped directory
              const files = await binding._collectFilesFromEntry(entry);
              if(files.length > 0) {
                // Create a new FileList-like object
                const dataTransfer = new DataTransfer();
                files.forEach(f => dataTransfer.items.add(f));
                $fileInput[0].files = dataTransfer.files;
                $fileInput.trigger('change');
              }
              return;
            }
          }
        }
      });
      
      if(debug) {
        console.log("Initialized directory input:", $el.attr('id'));
      }
    },

    getValue: function(el) {
      const $el = $(el);
      const uploadInfo = $el.data('uploadInfo');
      
      if(!uploadInfo) {
        return null;
      }
      
      return uploadInfo;
    },

    setValue: function(el, value) {
      // File inputs cannot be programmatically set for security reasons
    },

    subscribe: function(el, callback) {
      const $el = $(el);
      const $fileInput = $el.find('input[type="file"]');
      const $progress = $el.find('.progress');
      const $progressBar = $progress.find('.progress-bar');
      const binding = this;
      
      if(debug) {
        console.log('Subscribe - Found progress element:', $progress.length);
        console.log('Subscribe - Found progress bar:', $progressBar.length);
      }
      
      $fileInput.on('change.directoryInputBinding', async function(e) {
        const files = e.target.files;
        
        if(!files || files.length === 0) {
          $el.data('uploadInfo', null);
          callback(false);
          return;
        }

        // Filter out hidden files
        const filteredFiles = [];
        for(let i = 0; i < files.length; i++) {
          const file = files[i];
          const pathParts = (file.webkitRelativePath || file.name).split('/');
          const isHidden = pathParts.some(part => part.startsWith('.'));
          if(!isHidden) {
            filteredFiles.push(file);
          }
        }

        if(filteredFiles.length === 0) {
          $el.data('uploadInfo', null);
          callback(false);
          return;
        }

        // Show progress bar
        $progress.show();
        $progress.css('display', 'block');
        
        // Set initial progress
        $progressBar.text('0 / ' + filteredFiles.length + ' files');
        
        if(debug) {
          console.log('Progress bar shown, uploading', filteredFiles.length, 'files');
        }

        try {
          // Upload files and get the result
          const result = await binding._uploadFiles(el, filteredFiles, $progressBar, $progress);
          
          // Store the result
          $el.data('uploadInfo', result);
          
          // Hide progress bar after a delay to show 100% completion
          setTimeout(() => {
            $progress.hide();
          }, 1000);
          
          // Trigger callback
          callback(true);
          
        } catch(error) {
          console.error("Error uploading directory:", error);
          $progress.hide();
          alert("Error uploading directory: " + error.message);
        }
      });
    },

    _uploadFiles: async function(el, files, $progressBar, $progress) {
      const $el = $(el);
      const inputId = $el.attr('id');  // Get the main input ID from the outer div
      
      // Build directory structure
      const directoryStructure = {};
      const fileData = [];
      
      for(let i = 0; i < files.length; i++) {
        const file = files[i];
        const relativePath = file.webkitRelativePath || file.name;
        
        fileData.push({
          name: file.name,
          size: file.size,
          type: file.type,
          relativePath: relativePath,
          lastModified: file.lastModified
        });
        
        this._addToDirectoryTree(directoryStructure, relativePath, {
          name: file.name,
          size: file.size,
          type: file.type,
          lastModified: file.lastModified
        });
      }

      // Store relative paths separately
      const relativePaths = [];
      for(let i = 0; i < files.length; i++) {
        relativePaths.push(files[i].webkitRelativePath || files[i].name);
      }
      
      // Since Shiny's upload endpoint doesn't work with custom bindings,
      // we'll send files as base64 data through the normal setValue mechanism
      // Base64 encoding increases size by ~33%, so we need to be careful with memory
      
      const uploadedFiles = [];
      
      // Get max file size from data attribute (in bytes)
      // This is set by R based on maxSize parameter or shiny.maxRequestSize option
      const maxFileSizeAttr = $el.attr('data-max-file-size');
      const MAX_BASE64_SIZE = maxFileSizeAttr ? parseInt(maxFileSizeAttr, 10) : (5 * 1024 * 1024); // Default 5MB to match Shiny default
      
      for(let i = 0; i < files.length; i++) {
        const file = files[i];
        const relativePath = relativePaths[i];
        
        // Update progress with file count
        $progressBar.text((i + 1) + ' / ' + files.length + ' files');
        
        if(file.size <= MAX_BASE64_SIZE) {
          // Read file as base64 for small files
          const fileData = await new Promise((resolve, reject) => {
            const reader = new FileReader();
            reader.onload = function(e) {
              resolve(e.target.result); // This is the base64 data URL
            };
            reader.onerror = function() {
              reject(new Error('Failed to read file: ' + file.name));
            };
            reader.readAsDataURL(file);
          });
          
          uploadedFiles.push({
            name: file.name,
            size: file.size,
            type: file.type,
            datapath: null, // Will be created server-side
            relativePath: relativePath,
            base64data: fileData
          });
        } else {
          // For large files (>50MB), skip them with a warning
          console.warn('Skipping large file (>' + (MAX_BASE64_SIZE / 1024 / 1024) + 'MB):', file.name, '(' + (file.size / 1024 / 1024).toFixed(2) + 'MB)');
          
          uploadedFiles.push({
            name: file.name,
            size: file.size,
            type: file.type,
            datapath: null,
            relativePath: relativePath,
            base64data: null, // Too large for base64
            _tooLarge: true
          });
        }
      }
      
      // Set final progress
      $progressBar.text(uploadedFiles.length + ' / ' + files.length + ' files');

      // Return the structured data with base64 content
      return {
        name: uploadedFiles.map(f => f.name),
        size: uploadedFiles.map(f => f.size),
        type: uploadedFiles.map(f => f.type),
        relativePath: uploadedFiles.map(f => f.relativePath),
        base64data: uploadedFiles.map(f => f.base64data),
        directoryStructure: directoryStructure
      };
    },

    _collectFilesFromEntry: async function(entry, path = '') {
      const files = [];
      
      if(entry.isFile) {
        // Get the file
        const file = await new Promise((resolve, reject) => {
          entry.file(resolve, reject);
        });
        
        // Add relative path to the file object
        const fullPath = path ? path + '/' + file.name : file.name;
        Object.defineProperty(file, 'webkitRelativePath', {
          value: fullPath,
          writable: false
        });
        
        // Skip hidden files
        if(!file.name.startsWith('.')) {
          files.push(file);
        }
        
      } else if(entry.isDirectory) {
        const dirReader = entry.createReader();
        
        // Read all entries in this directory
        const entries = await new Promise((resolve, reject) => {
          const allEntries = [];
          
          function readEntries() {
            dirReader.readEntries(function(results) {
              if(results.length === 0) {
                resolve(allEntries);
              } else {
                allEntries.push(...results);
                readEntries();
              }
            }, reject);
          }
          
          readEntries();
        });
        
        // Recursively collect files from subdirectories
        const newPath = path ? path + '/' + entry.name : entry.name;
        for(const subEntry of entries) {
          // Skip hidden directories
          if(!subEntry.name.startsWith('.')) {
            const subFiles = await this._collectFilesFromEntry(subEntry, newPath);
            files.push(...subFiles);
          }
        }
      }
      
      return files;
    },

    _addToDirectoryTree: function(tree, path, fileInfo) {
      const parts = path.split('/');
      let current = tree;

      for(let i = 0; i < parts.length; i++) {
        const part = parts[i];
        const isLast = i === parts.length - 1;

        if(isLast) {
          // It's a file
          if(!current._files) {
            current._files = [];
          }
          current._files.push({
            name: part,
            size: fileInfo.size,
            type: fileInfo.type,
            lastModified: fileInfo.lastModified
          });
        } else {
          // It's a directory
          if(!current[part]) {
            current[part] = {};
          }
          current = current[part];
        }
      }
    },

    unsubscribe: function(el) {
      const $el = $(el);
      const $fileInput = $el.find('input[type="file"]');
      $fileInput.off('.directoryInputBinding');
    },

    getType: function(el) {
      return 'dipsaus.directoryInput';
    }
  });

  // Register the binding with higher priority than standard fileInput
  Shiny.inputBindings.register(directoryInputBinding, 'dipsaus.directoryInput', 100);

  if(debug) {
    console.log("Registered directoryInput binding");
  }
}
