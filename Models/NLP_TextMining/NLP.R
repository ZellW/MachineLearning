library(cleanNLP)
library(sotu)
library(dplyr)
library(tokenizers)
library(reticulate)
reticulate::use_python("../anaconda3/envs/NLP2/")

data(sotu_text)
data(sotu_meta)

txt <- c("You're invited to join Yext on Tuesday, April 23rd, in a private suite",
        "The Charlotte Knights take on the Toledo Mud Hens",
        "Minor League Baseball at BB&T Ballpark!",
        "We will be enjoying the game from the 3rd Base Suite which has fantastic views of the field as well as the Uptown Charlotte skyline.",
        "Plus we will have plenty of food and drinks to go around!")

writeLines(txt, tf <- tempfile())

cnlp_init_tokenizers()


######################

#https://rstudio.github.io/reticulate/articles/python_packages.html

py_config()

conda_create("NLP2")


  # Solving environment: ...working... done
  # 
  # 
  # ==> WARNING: A newer version of conda exists. <==
  #   current version: 4.5.11
  # latest version: 4.6.8
  # 
  # Please update conda by running
  # 
  # $ conda update -n base -c defaults conda
  # 
  #  
  # ## Package Plan ##
  # 
  # environment location: C:\Users\czbs7d\DOCUME~1\ANACON~1\envs\NLP2
  # 
  # added / updated specs: 
  #   - python
  # 
  # 
  # The following packages will be downloaded:
  #   
  #   package                    |            build
  # ---------------------------|-----------------
  #   setuptools-40.8.0          |           py37_0         663 KB
  # pip-19.0.3                 |           py37_0         1.8 MB
  # certifi-2019.3.9           |           py37_0         155 KB
  # openssl-1.1.1b             |       he774522_1         5.7 MB
  # python-3.7.2               |      h8c8aaf0_10        17.7 MB
  # ca-certificates-2019.1.23  |                0         158 KB
  # wheel-0.33.1               |           py37_0          57 KB
  # sqlite-3.27.2              |       he774522_0         941 KB
  # ------------------------------------------------------------
  #   Total:        27.2 MB
  # 
  # The following NEW packages will be INSTALLED:
  #   
  #   ca-certificates: 2019.1.23-0           
  # certifi:         2019.3.9-py37_0       
  # openssl:         1.1.1b-he774522_1     
  # pip:             19.0.3-py37_0         
  # python:          3.7.2-h8c8aaf0_10     
  # setuptools:      40.8.0-py37_0         
  # sqlite:          3.27.2-he774522_0     
  # vc:              14.1-h0510ff6_4       
  # vs2015_runtime:  14.15.26706-h3a45250_0
  # wheel:           0.33.1-py37_0         
  # wincertstore:    0.2-py37_0            
  # 
  # 
  # Downloading and Extracting Packages
  # setuptools-40.8.0    | 663 KB    | ########## | 100% 
  #   pip-19.0.3           | 1.8 MB    | ########## | 100% 
  #   certifi-2019.3.9     | 155 KB    | ########## | 100% 
  #   openssl-1.1.1b       | 5.7 MB    | ########## | 100% 
  #   python-3.7.2         | 17.7 MB   | ########## | 100% 
  #   ca-certificates-2019 | 158 KB    | ########## | 100% 
  #   wheel-0.33.1         | 57 KB     | ########## | 100% 
  #   sqlite-3.27.2        | 941 KB    | ########## | 100% 
  #   Preparing transaction: ...working... done
  # Verifying transaction: ...working... done
  # Executing transaction: ...working... done
  # #
  # # To activate this environment, use:
  # # > activate NLP2
  # #
  # # To deactivate an active environment, use:
  # # > deactivate
  # #
  # # * for power-users using bash, you must source
  # #

conda_install("NLP2", "spacy")

# Solving environment: ...working... done
# 
# 
# ==> WARNING: A newer version of conda exists. <==
#   current version: 4.5.11
# latest version: 4.6.8
# 
# Please update conda by running
# 
# $ conda update -n base -c defaults conda
# 
# 
# 
# ## Package Plan ##
# 
# environment location: C:\Users\czbs7d\DOCUME~1\ANACON~1\envs\NLP2
# 
# added / updated specs: 
#   - spacy
# 
# 
# The following packages will be downloaded:
#   
#   package                    |            build
# ---------------------------|-----------------
#   cffi-1.12.2                |   py37hb32ad35_1         218 KB  conda-forge
# spacy-2.1.3                |   py37he980bc4_0        54.6 MB  conda-forge
# intel-openmp-2019.3        |              203         1.7 MB
# six-1.12.0                 |        py37_1000          21 KB  conda-forge
# pycparser-2.19             |           py37_1         171 KB  conda-forge
# preshed-2.0.1              |   py37h33f27b4_0          70 KB
# pyopenssl-19.0.0           |           py37_0          81 KB  conda-forge
# pyrsistent-0.14.11         |   py37hfa6e2cd_0          89 KB  conda-forge
# asn1crypto-0.24.0          |        py37_1003         154 KB  conda-forge
# certifi-2019.3.9           |           py37_0         149 KB  conda-forge
# wasabi-0.2.0               |             py_0          18 KB  conda-forge
# cython-blis-0.2.4          |   py37hfa6e2cd_0         2.6 MB  conda-forge
# thinc-7.0.4                |   py37he980bc4_0         1.3 MB  conda-forge
# jsonschema-3.0.0a3         |        py37_1000          98 KB  conda-forge
# tqdm-4.31.1                |             py_0          40 KB  conda-forge
# attrs-19.1.0               |             py_0          32 KB  conda-forge
# plac-0.9.6                 |             py_1          18 KB  conda-forge
# murmurhash-1.0.0           |   py37h6538335_0          17 KB  conda-forge
# idna-2.8                   |        py37_1000         100 KB  conda-forge
# openssl-1.1.1b             |       hfa6e2cd_2         4.8 MB  conda-forge
# requests-2.21.0            |        py37_1000          84 KB  conda-forge
# numpy-1.16.2               |   py37h8078771_1         4.0 MB  conda-forge
# cymem-2.0.2                |   py37h74a9793_0          35 KB
# ca-certificates-2019.3.9   |       hecc5488_0         184 KB  conda-forge
# srsly-0.0.5                |   py37h6538335_0         183 KB  conda-forge
# win_inet_pton-1.1.0        |           py37_0           7 KB  conda-forge
# cryptography-2.6.1         |   py37h7a1dbc1_0         561 KB
# libcblas-3.8.0             |            4_mkl         3.5 MB  conda-forge
# libblas-3.8.0              |            4_mkl         3.5 MB  conda-forge
# liblapack-3.8.0            |            4_mkl         3.5 MB  conda-forge
# mkl-2019.1                 |              144       158.3 MB
# pysocks-1.6.8              |        py37_1002          22 KB  conda-forge
# urllib3-1.24.1             |        py37_1000         148 KB  conda-forge
# chardet-3.0.4              |        py37_1003         184 KB  conda-forge
# ------------------------------------------------------------
#   Total:       240.4 MB
# 
# The following NEW packages will be INSTALLED:
#   
#   asn1crypto:      0.24.0-py37_1003       conda-forge
# attrs:           19.1.0-py_0            conda-forge
# cffi:            1.12.2-py37hb32ad35_1  conda-forge
# chardet:         3.0.4-py37_1003        conda-forge
# cryptography:    2.6.1-py37h7a1dbc1_0              
# cymem:           2.0.2-py37h74a9793_0              
# cython-blis:     0.2.4-py37hfa6e2cd_0   conda-forge
# idna:            2.8-py37_1000          conda-forge
# intel-openmp:    2019.3-203                        
# jsonschema:      3.0.0a3-py37_1000      conda-forge
# libblas:         3.8.0-4_mkl            conda-forge
# libcblas:        3.8.0-4_mkl            conda-forge
# liblapack:       3.8.0-4_mkl            conda-forge
# mkl:             2019.1-144                        
# murmurhash:      1.0.0-py37h6538335_0   conda-forge
# numpy:           1.16.2-py37h8078771_1  conda-forge
# plac:            0.9.6-py_1             conda-forge
# preshed:         2.0.1-py37h33f27b4_0              
# pycparser:       2.19-py37_1            conda-forge
# pyopenssl:       19.0.0-py37_0          conda-forge
# pyrsistent:      0.14.11-py37hfa6e2cd_0 conda-forge
# pysocks:         1.6.8-py37_1002        conda-forge
# requests:        2.21.0-py37_1000       conda-forge
# six:             1.12.0-py37_1000       conda-forge
# spacy:           2.1.3-py37he980bc4_0   conda-forge
# srsly:           0.0.5-py37h6538335_0   conda-forge
# thinc:           7.0.4-py37he980bc4_0   conda-forge
# tqdm:            4.31.1-py_0            conda-forge
# urllib3:         1.24.1-py37_1000       conda-forge
# wasabi:          0.2.0-py_0             conda-forge
# win_inet_pton:   1.1.0-py37_0           conda-forge
# 
# The following packages will be UPDATED:
#   
#   ca-certificates: 2019.1.23-0                        --> 2019.3.9-hecc5488_0 conda-forge
# certifi:         2019.3.9-py37_0                    --> 2019.3.9-py37_0     conda-forge
# openssl:         1.1.1b-he774522_1                  --> 1.1.1b-hfa6e2cd_2   conda-forge
# 
# 
# Downloading and Extracting Packages
# cffi-1.12.2          | 218 KB    | ########## | 100% 
#   spacy-2.1.3          | 54.6 MB   | ########## | 100% 
#   intel-openmp-2019.3  | 1.7 MB    | ########## | 100% 
#   six-1.12.0           | 21 KB     | ########## | 100% 
#   pycparser-2.19       | 171 KB    | ########## | 100% 
#   preshed-2.0.1        | 70 KB     | ########## | 100% 
#   pyopenssl-19.0.0     | 81 KB     | ########## | 100% 
#   pyrsistent-0.14.11   | 89 KB     | ########## | 100% 
#   asn1crypto-0.24.0    | 154 KB    | ########## | 100% 
#   certifi-2019.3.9     | 149 KB    | ########## | 100% 
#   wasabi-0.2.0         | 18 KB     | ########## | 100% 
#   cython-blis-0.2.4    | 2.6 MB    | ########## | 100% 
#   thinc-7.0.4          | 1.3 MB    | ########## | 100% 
#   jsonschema-3.0.0a3   | 98 KB     | ########## | 100% 
#   tqdm-4.31.1          | 40 KB     | ########## | 100% 
#   attrs-19.1.0         | 32 KB     | ########## | 100% 
#   plac-0.9.6           | 18 KB     | ########## | 100% 
#   murmurhash-1.0.0     | 17 KB     | ########## | 100% 
#   idna-2.8             | 100 KB    | ########## | 100% 
#   openssl-1.1.1b       | 4.8 MB    | ########## | 100% 
#   requests-2.21.0      | 84 KB     | ########## | 100% 
#   numpy-1.16.2         | 4.0 MB    | ########## | 100% 
#   cymem-2.0.2          | 35 KB     | ########## | 100% 
#   ca-certificates-2019 | 184 KB    | ########## | 100% 
#   srsly-0.0.5          | 183 KB    | ########## | 100% 
#   win_inet_pton-1.1.0  | 7 KB      | ########## | 100% 
#   cryptography-2.6.1   | 561 KB    | ########## | 100% 
#   libcblas-3.8.0       | 3.5 MB    | ########## | 100% 
#   libblas-3.8.0        | 3.5 MB    | ########## | 100% 
#   liblapack-3.8.0      | 3.5 MB    | ########## | 100% 
#   mkl-2019.1           | 158.3 MB  | ########## | 100% 
#   pysocks-1.6.8        | 22 KB     | ########## | 100% 
#   urllib3-1.24.1       | 148 KB    | ########## | 100% 
#   chardet-3.0.4        | 184 KB    | ########## | 100% 
#   Preparing transaction: ...working... done
# Verifying transaction: ...working... done
# Executing transaction: ...working... done

reticulate::use_python("../anaconda3/envs/NLP2/")
py_config()

cnlp_init_spacy()

# https://github.com/statsmaths/cleanNLP/issues/10
reticulate::import("spacy")

https://github.com/explosion/spaCy/issues/1761


#####################

anno <- cnlp_annotate(tf)
names(anno)

cnlp_get_token(anno)

sotu <- cleanNLP::cnlp_annotate(sotu_text, as_strings = TRUE, meta = sotu_meta)

cnlp_get_token(sotu_text) %>%
  group_by(id, sid) %>%
  summarize(sent_len = n()) %$%
  quantile(sent_len, seq(0,1,0.1)txt