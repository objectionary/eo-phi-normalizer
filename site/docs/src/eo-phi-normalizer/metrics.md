# `eo-phi-normalizer metrics`

## Metrics

See [Metrics](../metrics.md) for the specification of metrics collected by this command.

## Environment

{{#include ../common/celsius.md}}

## CLI

### `--help`

```$ as console
eo-phi-normalizer metrics --help
```

```console
Usage: eo-phi-normalizer metrics [FILE] [-o|--output-file FILE]
                                 [-b|--bindings-path PATH]

  Collect metrics for a PHI program.

Available options:
  FILE                     FILE to read input from. When no FILE is specified,
                           read from stdin.
  -o,--output-file FILE    Output to FILE. When this option is not specified,
                           output to stdout.
  -b,--bindings-path PATH  Report metrics for bindings of a formation accessible
                           in a program by the PATH. When this option is not
                           specified, metrics for bindings are not reported.
                           Example of a PATH: 'org.eolang'.
  -h,--help                Show this help text
```

### `FILE`

```$ as json
eo-phi-normalizer metrics celsius.phi
```

```json
{
  "bindings-by-path-metrics": null,
  "program-metrics": {
    "applications": 4,
    "dataless": 1,
    "dispatches": 9,
    "formations": 3
  }
}
```

### `FILE` not specified (read from stdin)

```$ as json
cat celsius.phi | eo-phi-normalizer metrics
```

```json
{
  "bindings-by-path-metrics": null,
  "program-metrics": {
    "applications": 4,
    "dataless": 1,
    "dispatches": 9,
    "formations": 3
  }
}
```

### `--bindings-path`

```$ as console
eo-phi-normalizer metrics --bindings-path '' celsius.phi
```

```console
{
  "bindings-by-path-metrics": {
    "bindings-metrics": [
      {
        "metrics": {
          "applications": 2,
          "dataless": 0,
          "dispatches": 6,
          "formations": 0
        },
        "name": "c"
      },
      {
        "metrics": {
          "applications": 2,
          "dataless": 0,
          "dispatches": 3,
          "formations": 2
        },
        "name": "result"
      }
    ],
    "path": ""
  },
  "program-metrics": {
    "applications": 4,
    "dataless": 1,
    "dispatches": 9,
    "formations": 3
  }
}
```
