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
          totalSize: filesize
        });
        
        // Notify R that stream is ready
        Shiny.setInputValue('__dipsaus_stream_download_ready__', {
          download_id: downloadId,
          ready: true
        }, { priority: 'event' });
        
      } catch (e) {
        console.error('[dipsaus] Stream download start error:', e);
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
          console.error('[dipsaus] Download not found:', downloadId);
          return;
        }
        
        // Decode base64 chunk to Uint8Array
        const binaryString = atob(msg.chunk);
        const len = binaryString.length;
        const bytes = new Uint8Array(len);
        for (let i = 0; i < len; i++) {
          bytes[i] = binaryString.charCodeAt(i);
        }
        
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
          console.error('[dipsaus] Download not found for end:', downloadId);
          return;
        }
        
        if (debug) {
          console.log('[dipsaus] Stream download complete:', {
            downloadId,
            bytesWritten: download.bytesWritten
          });
        }
        
        // Close the writer
        download.writer.close();
        
        // Clean up
        activeDownloads.delete(downloadId);
        
        // Notify R that download is complete
        Shiny.setInputValue('__dipsaus_stream_download_complete__', {
          download_id: downloadId,
          bytes_written: download.bytesWritten
        }, { priority: 'event' });
        
      } catch (e) {
        console.error('[dipsaus] Stream download end error:', e);
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
        
      } catch (e) {
        console.error('[dipsaus] Stream download abort error:', e);
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
        }
        
        // Decode base64 chunk to Uint8Array
        const binaryString = atob(msg.chunk);
        const len = binaryString.length;
        const bytes = new Uint8Array(len);
        for (let i = 0; i < len; i++) {
          bytes[i] = binaryString.charCodeAt(i);
        }
        
        // Accumulate chunk
        window._dipsausBlobChunks[downloadId].push(bytes);
        
        // When complete, create blob and trigger download
        if (msg.is_complete === true) {
          const chunks = window._dipsausBlobChunks[downloadId];
          const blob = new Blob(chunks, { type: 'application/octet-stream' });
          const url = URL.createObjectURL(blob);
          const a = document.createElement('a');
          a.href = url;
          a.download = msg.filename || 'download';
          document.body.appendChild(a);
          a.click();
          document.body.removeChild(a);
          
          // Cleanup
          setTimeout(() => URL.revokeObjectURL(url), 100);
          delete window._dipsausBlobChunks[downloadId];
          
          if (debug) {
            console.log('[dipsaus] Blob download complete:', downloadId);
          }
        }
        
      } catch (e) {
        console.error('[dipsaus] Blob download error:', e);
      }
    });
    
  });
}
