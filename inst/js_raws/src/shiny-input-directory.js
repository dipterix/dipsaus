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

  // Run feature detection
  let supportsFileSystemAccessAPI = false;
  let supportsWebkitGetAsEntry = false;
  try {
    supportsFileSystemAccessAPI = 'getAsFileSystemHandle' in DataTransferItem.prototype;
    supportsWebkitGetAsEntry = 'webkitGetAsEntry' in DataTransferItem.prototype;
  } catch (e) {
    // Feature detection failed, both will remain false
  }

  if(debug) {
    console.log('File System Access API support:', supportsFileSystemAccessAPI);
    console.log('Webkit GetAsEntry support:', supportsWebkitGetAsEntry);
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
          // Try to collect files from the dropped item using modern API first
          const dataItems = [...items];
          const files = await binding._extractFilesFromDataItems(dataItems);
          
          if(files.length > 0) {
            // Create a new FileList-like object
            const dataTransfer = new DataTransfer();
            files.forEach(f => dataTransfer.items.add(f));
            $fileInput[0].files = dataTransfer.files;
            $fileInput.trigger('change');
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
        let totalSize = 0;
        for(let i = 0; i < files.length; i++) {
          const file = files[i];
          const pathParts = (file.webkitRelativePath || file.name).split('/');
          const isHidden = pathParts.some(part => part.startsWith('.'));
          if(!isHidden) {
            filteredFiles.push(file);
            totalSize += file.size;
          }
        }

        if(filteredFiles.length === 0) {
          $el.data('uploadInfo', null);
          callback(false);
          return;
        }
        
        // Check total size to prevent memory issues
        const maxFileSizeAttr = $el.attr('data-max-file-size');
        const MAX_BASE64_SIZE = maxFileSizeAttr ? parseInt(maxFileSizeAttr, 10) : (5 * 1024 * 1024);
        const maxTotalSize = MAX_BASE64_SIZE * filteredFiles.length;
        
        if(totalSize > maxTotalSize) {
          const totalSizeMB = (totalSize / 1024 / 1024).toFixed(2);
          const maxTotalSizeMB = (maxTotalSize / 1024 / 1024).toFixed(2);
          alert(`Total directory size (${totalSizeMB} MB) exceeds the maximum allowed (${maxTotalSizeMB} MB). Please select a smaller directory or increase the file size limit.`);
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
          try {
            const fileData = await new Promise((resolve, reject) => {
              const reader = new FileReader();
              reader.onload = function(e) {
                resolve(e.target.result); // This is the base64 data URL
              };
              reader.onerror = function(err) {
                reject(new Error('Failed to read file: ' + file.name));
              };
              // Add timeout to prevent hanging
              const timeout = setTimeout(() => {
                reader.abort();
                reject(new Error('Timeout reading file: ' + file.name));
              }, 30000); // 30 second timeout per file
              
              reader.readAsDataURL(file);
              reader.onloadend = function() {
                clearTimeout(timeout);
              };
            });
            
            uploadedFiles.push({
              name: file.name,
              size: file.size,
              type: file.type,
              datapath: null, // Will be created server-side
              relativePath: relativePath,
              base64data: fileData
            });
          } catch(err) {
            console.error('Error reading file:', file.name, err);
            uploadedFiles.push({
              name: file.name,
              size: file.size,
              type: file.type,
              datapath: null,
              relativePath: relativePath,
              base64data: null,
              _error: err.message
            });
          }
        } else {
          // For large files, skip them with a warning
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

    // Extract files from data transfer items using modern API with fallbacks
    _extractFilesFromDataItems: async function(dataItems) {
      const files = [];

      // Collect entries immediately
      const items = dataItems
        .filter(item => item.kind === 'file')
        .map(item => {
          if (supportsFileSystemAccessAPI && typeof item.getAsFileSystemHandle === 'function') {
            return {
              'type': 'FileSystemHandle',
              'entry': item.getAsFileSystemHandle() // Promise
            };
          }
          if (supportsWebkitGetAsEntry && typeof item.webkitGetAsEntry === 'function') {
            return {
              'type': 'WebkitEntry',
              'entry': item.webkitGetAsEntry()
            };
          }
          return {
            'type': 'Naive',
            'entry': item
          };
        });

      for (let i = 0; i < items.length; i++) {
        const item = items[i];

        if(!item || !item.entry) { continue; }

        switch (item.type) {
          case 'FileSystemHandle':
          {
            const entry = await item.entry;
            if(debug) {
              console.debug(`Using FileSystemAccessAPI to open [${ entry.name }]`);
            }
            const subFiles = await this._collectFilesFromFileSystemHandle(entry);
            files.push(...subFiles);
            break;
          }

          case 'WebkitEntry':
          {
            if(debug) {
              console.debug(`Using WebkitGetAsEntry to open [${ item.entry.name }]`);
            }
            const subFiles = await this._collectFilesFromWebkitEntry(item.entry);
            files.push(...subFiles);
            break;
          }

          default:
          {
            const file = item.entry.getAsFile();
            if(debug) {
              console.debug(`Using Naive method to open the file ${ file ? file.name : 'unknown' }`);
            }
            if(file && !file.name.startsWith('.')) {
              files.push(file);
            }
          }
        }
      }

      return files;
    },

    // Collect files from File System Access API handle
    _collectFilesFromFileSystemHandle: async function(entry, path = '') {
      const files = [];

      const handleEntry = async (e, currentPath) => {
        // Skip hidden files and directories
        if(e.name.startsWith('.')) {
          return;
        }

        if(e.kind === 'file') {
          const file = await e.getFile();
          
          // Add relative path to the file object
          const fullPath = currentPath ? currentPath + '/' + file.name : file.name;
          Object.defineProperty(file, 'webkitRelativePath', {
            value: fullPath,
            writable: false
          });
          
          files.push(file);
        } else if (e.kind === 'directory') {
          const newPath = currentPath ? currentPath + '/' + e.name : e.name;
          for await (const [name, child] of e.entries()) {
            await handleEntry(child, newPath);
          }
        }
      };

      await handleEntry(entry, path);
      return files;
    },

    // Collect files from webkit entry (legacy API)
    _collectFilesFromWebkitEntry: async function(entry, path = '') {
      const files = [];
      
      const handleEntry = async (e, currentPath) => {
        // Skip hidden files and directories
        if(e.name.startsWith('.')) {
          return;
        }

        if (e.isFile) {
          const file = await new Promise((resolve, reject) => {
            e.file((file) => {
              if(file) {
                // Add relative path to the file object
                const fullPath = currentPath ? currentPath + '/' + file.name : file.name;
                Object.defineProperty(file, 'webkitRelativePath', {
                  value: fullPath,
                  writable: false
                });
                resolve(file);
              } else {
                resolve(null);
              }
            }, resolve);
          });
          
          if(file) {
            files.push(file);
          }
        } else if (e.isDirectory) {
          const reader = e.createReader();
          const entries = await new Promise((resolve, reject) => {
            const allEntries = [];
            
            function readEntries() {
              reader.readEntries(async (results) => {
                if(results.length === 0) {
                  resolve(allEntries);
                } else {
                  allEntries.push(...results);
                  readEntries();
                }
              }, resolve);
            }
            
            readEntries();
          });
          
          const newPath = currentPath ? currentPath + '/' + e.name : e.name;
          for (const subEntry of entries) {
            await handleEntry(subEntry, newPath);
          }
        }
      };

      await handleEntry(entry, path);
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
