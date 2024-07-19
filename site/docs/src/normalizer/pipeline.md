# `normalizer pipeline`

This command is used in the [pipeline](./pipeline.md) script.

## `normalizer pipeline print-dataize-configs`

This commands reads the [pipeline configuration](../pipeline.md#pipeline-configuration) and prints configurations for the [normalizer dataize](./dataize.md).

```sh
source scripts/lib.sh
normalizer pipeline print-dataize-configs --single-line --strip-phi-prefix "$PIPELINE_PHI_INITIAL_DIR_RELATIVE/" --config "$PIPELINE_CONFIG_FILE"
```

## `normalizer pipeline prepare-tests`

This command reads the [pipeline configuration](../pipeline.md#pipeline-configuration), reads `original` EO files, filters tests there, and writes them to `filtered` and `yaml` files.

```sh
source scripts/lib.sh
normalizer pipeline prepare-tests --config "$PIPELINE_CONFIG_FILE"
```
