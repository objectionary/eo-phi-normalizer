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
Usage: eo-phi-normalizer metrics [-b|--bindings-path PATH]
                                 [-o|--output-file FILE] [FILE]

  Collect metrics for a PHI program.

Available options:
  -b,--bindings-path PATH  Report metrics for bindings of a formation accessible
                           in a program by the PATH. When this option is not
                           specified, metrics for bindings are not reported.
                           Example of a PATH: 'org.eolang'.
  -o,--output-file FILE    Output to FILE. When this option is not specified,
                           output to stdout.
  FILE                     FILE to read input from. When no FILE is specified,
                           read from stdin.
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
    "applications": 3,
    "dataless": 1,
    "dispatches": 6,
    "formations": 1
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
    "applications": 3,
    "dataless": 1,
    "dispatches": 6,
    "formations": 1
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
          "applications": 1,
          "dataless": 0,
          "dispatches": 3,
          "formations": 0
        },
        "name": "c"
      },
      {
        "metrics": {
          "applications": 2,
          "dataless": 0,
          "dispatches": 3,
          "formations": 0
        },
        "name": "result"
      }
    ],
    "path": ""
  },
  "program-metrics": {
    "applications": 3,
    "dataless": 1,
    "dispatches": 6,
    "formations": 1
  }
}
```
