## Migrations

These script contain migrations of model run options data from previous versions.

The way to read this is

* 0.0.12.R - contains migration from first naomi version containing model run options to 0.0.12 
* 0.0.13.R - contains migration from naomi version 0.0.12 to 0.0.13

Each migration function must take a list of options incoming as key value pairs and must output a list of updated options in the same form.
Keys match the `name` field in the model run options for each of the controls. Values depend on the type of input.

A new migration should be added whenever the format of the model run options changes in structure. i.e. a change in `name`, a change in `type`, a new control being added or a control being removed.


### Reference data

Reference data contains serialized model options as we expect them to come in for a particular version of naomi. These we use for testing that migrations work.
