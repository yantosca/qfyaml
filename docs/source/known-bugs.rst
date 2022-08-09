##########
Known Bugs
##########

This page lists known bugs in :program:`qfyaml`. See the `Github
issues <https://github.com/yantosca/qfyaml/issues>`_ page for updates
on their status.

=============
Version 0.3.2
=============

Error parsing categories
------------------------

We discovered an error parsing this YAML file, where the
:code:`wet_deposition` tag is more than 2 indentation levels behind
behind the previous line.

.. code-block:: yaml

   operations:
     transport:
       passive_species:
         CH3ITracer:
           long_name: Methyl_iodide
           mol_wt_in_g: 142.0
           lifetime_in_s: 4.32e5
           default_bkg_conc_in_vv: 1.0e-20
     wet_deposition:
       activate: true

This has now been fixed in qfyaml 0.3.3.

NOTE: For best results with qfyaml, we recommend formatting YAML files
so that they contain a consistent indentation level throughout the
file (i.e. such as 2 or 4 spaces).  Editors such as Emacs can do this easily.

=============
Version 0.3.0
=============

Error parsing YAML sequence items containing spaces
---------------------------------------------------

Items in YAML sequences containing spaces are not parsed properly.
For example:

.. code-block:: yaml

   hard_to_find_fruits:
     - Passion fruit
     - Star fruit

will be parsed as 4 separate items (:code:`Passion`, :code:`fruit`,
:code:`Star`, and :code:`fruit`) instead the expected 2 items
:code:`Passion fruit` and :code:`Star fruit`.  We will fix this in a
future version.

Workaround
----------

Use underscores instead of spaces in YAML sequence items:

.. code-block:: yaml

   hard_to_find_fruits:
   - Passion_fruit
   - Star_fruit

And then you can remove the underscores in post-processing.

Error returning long YAML sequences
-----------------------------------

It was discovered that only a subset long YAML sequences were being
returned.  Upon further investigation, a string variable in routine
:file:`Get_Fields_string` was found to be too short to hold all of the
stored data within a YAML variable.

We have fixed this behavior in :program:`qfyaml 0.3.1`.

Arrays passed to QFYAML_Add_Get had to be the same size as the data in
the YAML file
--------------------------

When passing an array to routine :file:`QFYAML_Add_Get`, an error
would be returned if the array was not the same length as the array or
sequence in the YAML file.

In :program:`qfyaml 0.3.1`, arrays that are larger than the size of
the data may be passed to :file:`QFYAML_Add_Get`.  This will let you
declare an array size that is sufficiently large in the calling
routine.  This can be especially useful if you do not know the size of
the data to be read in from the YAML file.
