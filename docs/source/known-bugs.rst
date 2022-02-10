##########
Known Bugs
##########

This page links to known bugs in :program:`qfyaml`. See the `Github
issues <http:s://github.com/yantosca/qfyaml/issues>`_ page for updates
on their status.

*************
Version 0.3.0
*************

Error parsing YAML sequence items containing spaces
===================================================

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
