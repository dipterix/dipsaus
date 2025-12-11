/**
 * Stream download module for dipsaus
 * Uses StreamSaver.js to push files to browsers
 */

import { document_ready } from './utils.js';
import streamSaver from './streamsaver.js';

// Configure StreamSaver mitm path (runs once when module loads)
let mitmConfigured = false;
function configureMitm() {
  if (mitmConfigured) return;
  mitmConfigured = true;
  
  // Find the dipsaus script to determine the base path
  const scripts = document.getElementsByTagName('script');
  for (let i = 0; i < scripts.length; i++) {
    if (scripts[i].src && scripts[i].src.indexOf('dipsaus-dipterix-lib.js') !== -1) {
      const basePath = scripts[i].src.replace('dipsaus-dipterix-lib.js', '');
      streamSaver.mitm = basePath + 'streamsaver/mitm.html?version=2.0.0';
      break;
    }
  }
}

// Store active downloads
const activeDownloads = new Map();

// Simple ID generator
function generateId() {
  return 'dl_' + Math.random().toString(36).substr(2, 9) + '_' + Date.now();
}

// Helper function to decode base64 to Uint8Array
function base64ToUint8Array(base64) {
  const binaryString = atob(base64);
  const len = binaryString.length;
  const bytes = new Uint8Array(len);
  for (let i = 0; i < len; i++) {
    bytes[i] = binaryString.charCodeAt(i);
  }
  return bytes;
}

// Helper function to show notifications (compatible with Shiny)
function showNotification(message, type = 'error', duration = null) {
  // Try Shiny notification first
  if (typeof Shiny !== 'undefined' && Shiny.notifications && Shiny.notifications.show) {
    Shiny.notifications.show({
      html: message,
      type: type,
      duration: duration
    });
    return;
  }
  
  // Fallback to console
  if (type === 'error') {
    console.error('[dipsaus]', message);
  } else {
    console.log('[dipsaus]', message);
  }
}

export function register_streamDownload(Shiny, debug = false) {
  
  document_ready(() => {
    
    // Configure mitm path once DOM is ready
    configureMitm();
    
    if (debug) {
      console.log('[dipsaus] Registering stream download handlers');
      console.log('[dipsaus] StreamSaver mitm path:', streamSaver.mitm);
    }
    
    // Handler: Start a new stream download
    Shiny.addCustomMessageHandler('dipsaus.streamDownloadStart', (msg) => {
      try {
        const downloadId = msg.download_id || generateId();
        const filename = msg.filename || 'download';
        const filesize = msg.size || null;
        
        if (debug) {
          console.log('[dipsaus] Starting stream download:', { downloadId, filename, filesize });
        }
        
        // Check if StreamSaver is available and configured
        if (!streamSaver || !streamSaver.createWriteStream) {
          throw new Error('StreamSaver.js is not properly loaded');
        }
        
        // Create write stream
        const opts = {};
        if (filesize) {
          opts.size = filesize;
        }
        
        const fileStream = streamSaver.createWriteStream(filename, opts);
        const writer = fileStream.getWriter();
        
        // Store the writer for later chunk writes
        activeDownloads.set(downloadId, {
          writer: writer,
          filename: filename,
          bytesWritten: 0,
          totalSize: filesize,
          startTime: Date.now()
        });
        
        // Notify R that stream is ready
        Shiny.setInputValue('__dipsaus_stream_download_ready__', {
          download_id: downloadId,
          ready: true
        }, { priority: 'event' });
        
      } catch (e) {
        console.error('[dipsaus] Stream download start error:', e);
        showNotification('Failed to start download: ' + e.message, 'error');
        Shiny.setInputValue('__dipsaus_stream_download_error__', {
          download_id: msg.download_id,
          error: e.message
        }, { priority: 'event' });
      }
    });
    
    // Handler: Write a chunk to the stream
    Shiny.addCustomMessageHandler('dipsaus.streamDownloadChunk', (msg) => {
      try {
        const downloadId = msg.download_id;
        const download = activeDownloads.get(downloadId);
        
        if (!download) {
          throw new Error('No active download found for ID: ' + downloadId);
        }
        
        // Decode base64 chunk to Uint8Array
        const bytes = base64ToUint8Array(msg.chunk);
        
        // Write to stream
        download.writer.write(bytes);
        download.bytesWritten += bytes.length;
        
        if (debug && msg.chunk_index % 10 === 0) {
          console.log('[dipsaus] Stream chunk written:', {
            downloadId,
            chunk: msg.chunk_index,
            bytesWritten: download.bytesWritten
          });
        }
        
      } catch (e) {
        console.error('[dipsaus] Stream download chunk error:', e);
        showNotification('Download failed while writing data: ' + e.message, 'error');
        
        // Try to abort the download
        const download = activeDownloads.get(msg.download_id);
        if (download && download.writer) {
          try {
            download.writer.abort();
          } catch (abortError) {
            console.error('[dipsaus] Failed to abort stream:', abortError);
          }
        }
        activeDownloads.delete(msg.download_id);
        
        Shiny.setInputValue('__dipsaus_stream_download_error__', {
          download_id: msg.download_id,
          error: e.message
        }, { priority: 'event' });
      }
    });
    
    // Handler: End the stream download
    Shiny.addCustomMessageHandler('dipsaus.streamDownloadEnd', (msg) => {
      try {
        const downloadId = msg.download_id;
        const download = activeDownloads.get(downloadId);
        
        if (!download) {
          console.warn('[dipsaus] No active download to complete for ID:', downloadId);
          return;
        }
        
        // Close the writer
        download.writer.close();
        
        const duration = (Date.now() - download.startTime) / 1000;
        const sizeMB = (download.bytesWritten / (1024 * 1024)).toFixed(2);
        
        if (debug) {
          console.log('[dipsaus] Stream download complete:', {
            downloadId,
            filename: download.filename,
            bytesWritten: download.bytesWritten,
            duration: duration + 's',
            speed: (download.bytesWritten / duration / 1024 / 1024).toFixed(2) + ' MB/s'
          });
        }
        
        // Clean up
        activeDownloads.delete(downloadId);
        
        // Notify R that download is complete
        Shiny.setInputValue('__dipsaus_stream_download_complete__', {
          download_id: downloadId,
          bytes_written: download.bytesWritten
        }, { priority: 'event' });
        
      } catch (e) {
        console.error('[dipsaus] Stream download end error:', e);
        showNotification('Failed to complete download: ' + e.message, 'error');
        activeDownloads.delete(msg.download_id);
        Shiny.setInputValue('__dipsaus_stream_download_error__', {
          download_id: msg.download_id,
          error: e.message
        }, { priority: 'event' });
      }
    });
    
    // Handler: Abort a stream download
    Shiny.addCustomMessageHandler('dipsaus.streamDownloadAbort', (msg) => {
      try {
        const downloadId = msg.download_id;
        const download = activeDownloads.get(downloadId);
        
        if (download) {
          if (debug) {
            console.log('[dipsaus] Stream download aborted:', downloadId);
          }
          download.writer.abort(msg.reason || 'Download aborted');
          activeDownloads.delete(downloadId);
        }
        
        const reason = msg.reason || 'Unknown error';
        console.error('[dipsaus] Download aborted:', reason);
        showNotification('Download aborted: ' + reason, 'error');
        
      } catch (e) {
        console.error('[dipsaus] Stream download abort error:', e);
        activeDownloads.delete(msg.download_id);
      }
    });
    
    // Fallback handler for blob-based downloads (non-streaming)
    Shiny.addCustomMessageHandler('dipsaus.blobDownloadChunk', (msg) => {
      try {
        const downloadId = msg.filename || 'download';
        
        // Initialize chunks array for this download
        if (msg.chunk_index === 1) {
          window._dipsausBlobChunks = window._dipsausBlobChunks || {};
          window._dipsausBlobChunks[downloadId] = [];
          
          if (debug) {
            console.log('[dipsaus] Starting blob download:', downloadId);
          }
        }
        
        // Decode and accumulate chunk (skip if empty completion message)
        if (msg.chunk && msg.chunk.length > 0) {
          const bytes = base64ToUint8Array(msg.chunk);
          window._dipsausBlobChunks[downloadId].push(bytes);
        }
        
        // When complete, create blob and trigger download
        if (msg.is_complete === true) {
          const chunks = window._dipsausBlobChunks[downloadId];
          
          if (!chunks || chunks.length === 0) {
            throw new Error('No data received for download');
          }
          
          // Create blob from accumulated chunks
          const blob = new Blob(chunks, { type: 'application/octet-stream' });
          const url = URL.createObjectURL(blob);
          const a = document.createElement('a');
          a.href = url;
          a.download = msg.filename || 'download';
          a.style.display = 'none';
          document.body.appendChild(a);
          a.click();
          document.body.removeChild(a);
          
          // Cleanup
          setTimeout(() => URL.revokeObjectURL(url), 1000);
          delete window._dipsausBlobChunks[downloadId];
          
          if (debug) {
            console.log('[dipsaus] Blob download complete:', downloadId);
          }
        }
        
      } catch (e) {
        console.error('[dipsaus] Blob download error:', e);
        showNotification('Download failed: ' + e.message, 'error');
        
        // Cleanup on error
        if (window._dipsausBlobChunks && msg.filename) {
          delete window._dipsausBlobChunks[msg.filename];
        }
      }
    });
    
    if (debug) {
      console.log('[dipsaus] Stream download handlers registered');
    }
    
  });
}
