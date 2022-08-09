# Configuration file for the Sphinx documentation builder.
#
# This file only contains a selection of the most common options. For a full
# list see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
# import os
# import sys
# sys.path.insert(0, os.path.abspath('.'))

master_doc = 'index'

# -- Project information -----------------------------------------------------

project = 'qfyaml'
copyright = '2022, Bob Yantosca'
author = 'Bob Yantosca'

# The full version, including alpha/beta/rc tags
release = '0.4.2'

# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
html_theme = 'sphinx_rtd_theme'
#html_theme = 'alabaster'

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ['_static']

# Display GEOS-Chem logo
html_favicon = '_static/qfyaml-favicon.ico'
html_logo = "_static/qfyaml.png"
html_theme_options = {
    'logo_only': True,
    'display_version': True,
    'style_nav_header_background': '#FCFCFC',
    'prev_next_buttons_location': 'bottom',
}
