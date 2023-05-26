# Current submission:

Self check showed zero error/warning/note

Reverse dependency checks passed too.

# Additional comments:

Fixed the following two issues that might violate CRAN policy:

* Actively clear the cache created from (`R_user_dir`) when no files are used
* Removed `cpp11` from compiling flags and replaced obsolete usages
