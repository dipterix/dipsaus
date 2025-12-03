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
      
      // Add auto-cleanup checkbox handler
      const $autoCleanupCheckbox = $el.find('.dipsaus-auto-cleanup-checkbox');
      $autoCleanupCheckbox.on('change', function() {
        const isChecked = $(this).prop('checked');
        $el.attr('data-auto-cleanup', isChecked ? 'true' : 'false');
        if(debug) {
          console.log('Auto-cleanup toggled:', isChecked);
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
    
    getFileStatus: function(el) {
      const $el = $(el);
      return $el.data('fileStatus') || null;
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

        // Show progress bar
        $progress.show();
        $progress.css('display', 'block');
        
        // Set initial progress
        $progressBar.text('0 / ' + filteredFiles.length + ' files');
        
        if(debug) {
          console.log('Progress bar shown, uploading', filteredFiles.length, 'files');
        }

        try {
          // Process files incrementally
          await binding._processFilesIncremental(el, filteredFiles, $progressBar, $progress, callback);
          
        } catch(error) {
          console.error("Error uploading directory:", error);
          $progress.hide();
          alert("Error uploading directory: " + error.message);
        }
      });
    },

    _processFilesIncremental: async function(el, files, $progressBar, $progress, callback) {
      const $el = $(el);
      const inputId = $el.attr('id');
      
      // Build directory structure and file metadata
      const directoryStructure = {};
      const fileMetadata = [];
      
      for(let i = 0; i < files.length; i++) {
        const file = files[i];
        const relativePath = file.webkitRelativePath || file.name;
        
        const fileId = `file_${i}`;
        
        fileMetadata.push({
          fileId: fileId,
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
          lastModified: file.lastModified,
          fileId: fileId
        });
      }
      
      // Initialize file status tracking
      const fileStatus = {};
      fileMetadata.forEach(fm => {
        fileStatus[fm.fileId] = {
          name: fm.name,
          relativePath: fm.relativePath,
          status: 'pending',
          progress: 0,
          error: null
        };
      });
      
      // Read autoCleanup attribute
      const autoCleanupAttr = $el.attr('data-auto-cleanup');
      const autoCleanup = autoCleanupAttr === 'true';
      
      // Store initial metadata and status
      const initialData = {
        fileMetadata: fileMetadata,
        directoryStructure: directoryStructure,
        totalFiles: files.length,
        ready: false,
        upload_status: 'initialized',
        autoCleanup: autoCleanup
      };
      
      $el.data('uploadInfo', initialData);
      $el.data('fileStatus', fileStatus);
      
      // Trigger initial callback to send metadata
      callback(true);
      
      // Initialize progress2 if enabled
      const progressEnabled = $el.attr('data-progress-enabled') === 'true';
      const progressTitle = $el.attr('data-progress-title') || 'Uploading directory';
      let progressId = null;
      
      if(debug) {
        console.log('Progress settings:');
        console.log('  data-progress-enabled:', $el.attr('data-progress-enabled'));
        console.log('  progressEnabled:', progressEnabled);
        console.log('  progressTitle:', progressTitle);
        console.log('  window.Shiny available:', !!window.Shiny);
      }
      
      if(progressEnabled && window.Shiny) {
        progressId = `${inputId}_upload_progress`;
        if(debug) {
          console.log('Initializing progress2 with ID:', progressId);
        }
        Shiny.setInputValue(progressId + '_start', {
          title: progressTitle,
          max: files.length,
          shiny: true,
          quiet: false
        }, {priority: 'event'});
      }
      
      // Now process files one by one
      const maxFileSizeAttr = $el.attr('data-max-file-size');
      const MAX_TOTAL_FILE_SIZE = maxFileSizeAttr ? parseInt(maxFileSizeAttr, 10) : (5 * 1024 * 1024);
      const CHUNK_SIZE = 5 * 1024 * 1024;
      
      if(debug) {
        console.log('Processing files with settings:');
        console.log('  data-max-file-size attribute:', maxFileSizeAttr);
        console.log('  MAX_TOTAL_FILE_SIZE:', MAX_TOTAL_FILE_SIZE, 'bytes (', (MAX_TOTAL_FILE_SIZE / 1024 / 1024).toFixed(2), 'MB )');
        console.log('  CHUNK_SIZE:', CHUNK_SIZE, 'bytes');
      }
      
      for(let i = 0; i < files.length; i++) {
        const file = files[i];
        const fileId = `file_${i}`;
        const relativePath = file.webkitRelativePath || file.name;
        
        // Update progress bar
        $progressBar.text(`Processing ${i + 1} / ${files.length} files`);
        
        // Update status to processing
        fileStatus[fileId].status = 'processing';
        this._updateFileStatus(el, fileId, fileStatus[fileId]);
        
        try {
          let fileData = null;
          
          if(debug) {
            console.log(`File ${i + 1}: ${file.name}, size: ${file.size} bytes (${(file.size / 1024 / 1024).toFixed(2)} MB), max allowed: ${MAX_TOTAL_FILE_SIZE}`);
          }
          
          if(file.size <= MAX_TOTAL_FILE_SIZE) {
            // Check if file needs chunking (> 5MB)
            if(file.size > CHUNK_SIZE) {
              // Read file in chunks
              const chunks = [];
              const numChunks = Math.ceil(file.size / CHUNK_SIZE);
              
              for(let chunkIndex = 0; chunkIndex < numChunks; chunkIndex++) {
                const start = chunkIndex * CHUNK_SIZE;
                const end = Math.min(start + CHUNK_SIZE, file.size);
                const chunk = file.slice(start, end);
                
                const chunkData = await new Promise((resolve, reject) => {
                  const reader = new FileReader();
                  reader.onload = function(e) {
                    resolve(e.target.result);
                  };
                  reader.onerror = function(err) {
                    reject(new Error('Failed to read file chunk: ' + file.name));
                  };
                  const timeout = setTimeout(() => {
                    reader.abort();
                    reject(new Error('Timeout reading file chunk: ' + file.name));
                  }, 30000);
                  
                  reader.readAsDataURL(chunk);
                  reader.onloadend = function() {
                    clearTimeout(timeout);
                  };
                });
                
                chunks.push(chunkData);
                
                // Update progress
                fileStatus[fileId].progress = ((chunkIndex + 1) / numChunks) * 100;
                this._updateFileStatus(el, fileId, fileStatus[fileId]);
              }
              
              fileData = {
                fileId: fileId,
                name: file.name,
                size: file.size,
                type: file.type,
                relativePath: relativePath,
                base64data: chunks,
                chunked: true,
                numChunks: numChunks
              };
            } else {
              // Read small file as single base64 string
              const base64String = await new Promise((resolve, reject) => {
                const reader = new FileReader();
                reader.onload = function(e) {
                  resolve(e.target.result);
                };
                reader.onerror = function(err) {
                  reject(new Error('Failed to read file: ' + file.name));
                };
                const timeout = setTimeout(() => {
                  reader.abort();
                  reject(new Error('Timeout reading file: ' + file.name));
                }, 30000);
                
                reader.readAsDataURL(file);
                reader.onloadend = function() {
                  clearTimeout(timeout);
                };
              });
              
              fileData = {
                fileId: fileId,
                name: file.name,
                size: file.size,
                type: file.type,
                relativePath: relativePath,
                base64data: base64String,
                chunked: false
              };
              
              fileStatus[fileId].progress = 100;
            }
            
            // Update status to complete
            fileStatus[fileId].status = 'complete';
            fileStatus[fileId].progress = 100;
            
          } else {
            // File too large
            fileStatus[fileId].status = 'skipped';
            fileStatus[fileId].error = 'File exceeds maximum size';
            
            fileData = {
              fileId: fileId,
              name: file.name,
              size: file.size,
              type: file.type,
              relativePath: relativePath,
              base64data: null,
              _tooLarge: true
            };
          }
          
          // Send individual file data to Shiny
          this._sendFileToShiny(el, fileData);
          
          // Update file status
          this._updateFileStatus(el, fileId, fileStatus[fileId]);
          
          // Update progress2 if enabled
          if(progressEnabled && progressId && window.Shiny) {
            if(debug) {
              console.log('Sending progress update:', (i + 1), '/', files.length, '-', file.name);
            }
            Shiny.setInputValue(progressId + '_update', {
              inc: 1,
              current: i + 1,
              total: files.length,
              filename: file.name
            }, {priority: 'event'});
          }
          
        } catch(err) {
          console.error('Error processing file:', file.name, err);
          fileStatus[fileId].status = 'error';
          fileStatus[fileId].error = err.message;
          
          // Send error info to Shiny
          this._sendFileToShiny(el, {
            fileId: fileId,
            name: file.name,
            size: file.size,
            type: file.type,
            relativePath: relativePath,
            base64data: null,
            _error: err.message
          });
          
          this._updateFileStatus(el, fileId, fileStatus[fileId]);
        }
      }
      
      // All files processed
      $progress.hide();
      initialData.ready = true;
      
      // Determine upload status based on file processing results
      const hasErrors = Object.values(fileStatus).some(status => status.status === 'error');
      initialData.upload_status = hasErrors ? 'errored' : 'completed';
      
      $el.data('uploadInfo', initialData);
      
      // Close progress2 if enabled
      if(progressEnabled && progressId && window.Shiny) {
        if(debug) {
          console.log('Closing progress2:', progressId);
        }
        Shiny.setInputValue(progressId + '_close', true, {priority: 'event'});
      }
      
      // Trigger a final update to the main input with completed status
      // This allows R to know all files are done
      if(debug) {
        console.log('All files processed, updating main input with completed status');
      }
      Shiny.setInputValue(inputId + ':dipsaus.directoryInput', initialData, {priority: 'event'});
      
      callback(true);
    },
    
    _sendFileToShiny: function(el, fileData) {
      const $el = $(el);
      const inputId = $el.attr('id');
      const fileInputId = `${inputId}__file`;
      
      if(debug) {
        console.log('Sending file to Shiny:', fileData.name);
        console.log('  Has base64data:', fileData.base64data !== null && fileData.base64data !== undefined);
        console.log('  Chunked:', fileData.chunked || false);
        console.log('  _tooLarge:', fileData._tooLarge || false);
        console.log('  _error:', fileData._error || 'none');
      }
      
      if(window.Shiny && Shiny.setInputValue) {
        // Send with type suffix to trigger custom input handler
        Shiny.setInputValue(fileInputId + ':dipsaus.directoryInput.file', fileData, {
          priority: 'event'
        });
      }
    },
    
    _updateFileStatus: function(el, fileId, statusData) {
      const $el = $(el);
      const inputId = $el.attr('id');
      const statusInputId = `${inputId}__status`;
      const fileStatus = $el.data('fileStatus');
      
      if(window.Shiny && Shiny.setInputValue) {
        // Send updated status table as array
        const statusTable = Object.keys(fileStatus).map(fid => {
          const status = fileStatus[fid];
          return {
            fileId: fid,
            name: status.name,
            relativePath: status.relativePath,
            status: status.status,
            progress: status.progress,
            error: status.error !== null && status.error !== undefined ? String(status.error) : ""
          };
        });
        
        // Send with type suffix to trigger custom input handler
        Shiny.setInputValue(statusInputId + ':dipsaus.directoryInput.status', statusTable, {
          priority: 'event'
        });
      }
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
      const MAX_TOTAL_FILE_SIZE = maxFileSizeAttr ? parseInt(maxFileSizeAttr, 10) : (5 * 1024 * 1024); // Default 5MB to match Shiny default
      
      // Chunk size for large files (5MB)
      const CHUNK_SIZE = 5 * 1024 * 1024;
      
      for(let i = 0; i < files.length; i++) {
        const file = files[i];
        const relativePath = relativePaths[i];
        
        // Update progress with file count
        $progressBar.text((i + 1) + ' / ' + files.length + ' files');
        
        if(file.size <= MAX_TOTAL_FILE_SIZE) {
          try {
            // Check if file needs chunking (> 5MB)
            if(file.size > CHUNK_SIZE) {
              // Read file in chunks to avoid string length limitations
              const chunks = [];
              const numChunks = Math.ceil(file.size / CHUNK_SIZE);
              
              for(let chunkIndex = 0; chunkIndex < numChunks; chunkIndex++) {
                const start = chunkIndex * CHUNK_SIZE;
                const end = Math.min(start + CHUNK_SIZE, file.size);
                const chunk = file.slice(start, end);
                
                const chunkData = await new Promise((resolve, reject) => {
                  const reader = new FileReader();
                  reader.onload = function(e) {
                    resolve(e.target.result); // This is the base64 data URL
                  };
                  reader.onerror = function(err) {
                    reject(new Error('Failed to read file chunk: ' + file.name));
                  };
                  // Add timeout to prevent hanging
                  const timeout = setTimeout(() => {
                    reader.abort();
                    reject(new Error('Timeout reading file chunk: ' + file.name));
                  }, 30000); // 30 second timeout per chunk
                  
                  reader.readAsDataURL(chunk);
                  reader.onloadend = function() {
                    clearTimeout(timeout);
                  };
                });
                
                chunks.push(chunkData);
              }
              
              uploadedFiles.push({
                name: file.name,
                size: file.size,
                type: file.type,
                datapath: null, // Will be created server-side
                relativePath: relativePath,
                base64data: chunks, // Array of base64 chunks
                chunked: true,
                numChunks: numChunks
              });
            } else {
              // Read file as single base64 string for small files
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
                base64data: fileData,
                chunked: false
              });
            }
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
          // For files exceeding the total size limit, skip them with a warning
          console.warn('Skipping large file (>' + (MAX_TOTAL_FILE_SIZE / 1024 / 1024) + 'MB):', file.name, '(' + (file.size / 1024 / 1024).toFixed(2) + 'MB)');
          
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

      // Read autoCleanup attribute
      const autoCleanupAttr = $el.attr('data-auto-cleanup');
      const autoCleanup = autoCleanupAttr === 'true';

      // Return the structured data with base64 content
      return {
        name: uploadedFiles.map(f => f.name),
        size: uploadedFiles.map(f => f.size),
        type: uploadedFiles.map(f => f.type),
        relativePath: uploadedFiles.map(f => f.relativePath),
        base64data: uploadedFiles.map(f => f.base64data),
        directoryStructure: directoryStructure,
        autoCleanup: autoCleanup
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
