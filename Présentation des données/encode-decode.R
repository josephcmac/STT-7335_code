#----------------------------------------------------
# Example encrypt_file and decrypt_file functions
# (from your previous definition for convenience)
#----------------------------------------------------
library(openssl)

encrypt_file <- function(input_file, output_file, key) {
  data_raw <- readBin(input_file, what = "raw", n = file.info(input_file)$size)
  key_bytes <- sha256(charToRaw(key))
  iv <- rand_bytes(16)
  encrypted_data <- aes_cbc_encrypt(data_raw, key = key_bytes, iv = iv)
  out_conn <- file(output_file, "wb")
  writeBin(iv, con = out_conn)
  writeBin(as.vector(encrypted_data), con = out_conn)
  close(out_conn)
  message("Encrypted: ", input_file, " --> ", output_file)
}

decrypt_file <- function(encrypted_file, output_file, key) {
  file_size <- file.info(encrypted_file)$size
  in_conn <- file(encrypted_file, "rb")
  enc_raw <- readBin(in_conn, what = "raw", n = file_size)
  close(in_conn)
  iv <- enc_raw[1:16]
  ciphertext <- enc_raw[-(1:16)]
  key_bytes <- sha256(charToRaw(key))
  decrypted_data <- aes_cbc_decrypt(ciphertext, key = key_bytes, iv = iv)
  writeBin(decrypted_data, output_file)
  message("Decrypted: ", encrypted_file, " --> ", output_file)
}

#----------------------------------------------------
# Function to Process All Files in a Folder Recursively
#    FUN can be either encrypt_file or decrypt_file
#----------------------------------------------------
process_folder_recursively <- function(FUN, input_dir, output_dir, key) {
  
  # Get all files (recursive = TRUE)
  # 'full.names = TRUE' returns the full path to each file
  files <- list.files(input_dir, recursive = TRUE, full.names = TRUE)
  
  # Iterate over each path found
  for (path_in in files) {
    
    # Skip directories; we only process regular files
    if (dir.exists(path_in)) {
      next
    }
    
    # Compute the portion of the path that is relative to 'input_dir'
    # This helps replicate the sub-folder structure in 'output_dir'
    rel_path <- sub(paste0("^", normalizePath(input_dir)), "", normalizePath(path_in))
    
    # On some systems, the relative path might begin with a slash/backslash; remove it if present
    if (startsWith(rel_path, "/") || startsWith(rel_path, "\\")) {
      rel_path <- substring(rel_path, 2)
    }
    
    # Construct the corresponding output path
    path_out <- file.path(output_dir, rel_path)
    
    # Ensure the directory structure exists in the output directory
    dir.create(dirname(path_out), recursive = TRUE, showWarnings = FALSE)
    
    # Call the desired function (either encrypt or decrypt)
    FUN(path_in, path_out, key)
  }
  
  message("All files processed from '", input_dir, "' to '", output_dir, "'.")
}

key <-"fsd1&1f*Rk82&^7r#mREWWewl"

process_folder_recursively(
  FUN        = encrypt_file,      # the function to use
# FUN        = decrypt_file,       # the function to use
  input_dir  = "raw_data",  # folder containing files/sub-folders to encrypt
  output_dir = "raw_data_encrypted", 
  key        = key
)

