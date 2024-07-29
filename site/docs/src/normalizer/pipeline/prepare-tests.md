# `normalizer pipeline prepare-tests`

## `--config`

This command reads the [pipeline configuration](../pipeline.md#pipeline-configuration), reads `original` EO files, filters tests there, and writes them to `filtered` and `yaml` files.

```sh
source scripts/lib.sh
normalizer pipeline prepare-tests --config "$PIPELINE_CONFIG_FILE"
```
