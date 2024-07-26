# `normalizer report`

## Reports

The `normalizer report` command generates reports about initial and normalized `PHI` programs.

The reports contain detailed information about metrics collected for these programs.

The reports are in `HTML`, `GitHub Flavored Markdown`, and `JSON` formats.

## Environment

The command requires that there are:

- Initial `PHI` programs
- Normalized `PHI` programs
- A report configuration file

### `PHI` programs

Currently, we translate `EO` programs and get initial `PHI` programs.

Next, we normalize these `PHI` programs and get normalized `PHI` programs.

### Report configuration

The report configuration is stored in the `report` object in the [pipeline configuration file](../pipeline.md#pipeline-configuration).

## CLI

### `--help`

```$ as console
normalizer report --help
```

```console
Usage: normalizer report (-c|--config FILE)

  Generate reports about initial and normalized PHI programs.

Available options:
  -c,--config FILE         The FILE with a report configuration.
  -h,--help                Show this help text
```

### `--config`

```$ as console
normalizer report --config pipeline/config.yaml
```
