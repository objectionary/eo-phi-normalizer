# Quick start

## Install `normalizer` from the repository

See [Installation](./installation.md).

## Install dependencies

Install [NodeJS](https://nodejs.org/en).

```$ as console
node --version
```

Install [Java](https://www.java.com/en/download/help/download_options.html).

```$ as console
java --version
```

## Run normalizer

See [normalizer](./normalizer.md) for supported commands.

## Run pipeline

### Pipeline stages

The pipeline has several stages:

- Transform original `EO` programs into initial `EO` programs
- Translate initial `EO` programs to initial `PHI` programs
- Translate initial `PHI` programs to non-normalized `EO` programs
- Test `EO` programs
- Normalize initial `PHI` programs and get normalized `PHI` programs
- Translate normalized `PHI` programs to normalized `EO` programs
- Test normalized `EO` programs
- Report metrics on initial `PHI` programs and normalized `PHI` programs

### Environment

Current directory should be the root directory of the `normalizer` repo.

```console
git clone https://github.com/objectionary/normalizer --recurse-submodules
cd normalizer
```

### Modify pipeline configuration

The `pipeline/config.yaml` file specifies how original `EO` programs are transformed into initial `EO` programs.
Transformation is necessary because some objects in original `EO` programs don't work.
An initial `EO` program is an original `EO` program with some top-level objects excluded.

- `yamlDirectory` - a directory with transformed programs in `YAML` format
- `sets`
  - `source` - a file with an original `EO` program
  - `yaml` - a file for the initial `EO` program in `YAML` format
  - `destination` - a file for the initial `EO` program
  - `enable` - whether to process this entry of `sets`
  - `include` - a list of names of top-level object in the original `EO` program that should be included into the initial `EO` program
  - `exclude` - a list of names of top-level objects in the original `EO` program that shouldn't be included into the initial `EO` program

### Run script

Run `scripts/pipeline.sh`.

The script will run for several minutes and write the following entries to the `pipeline` directory:.

- `yaml` - . `EO` programs in `YAML` format
- `eo` - non-normalized `EO` programs
- `phi` - initial `PHI` programs
- `eo-non-normalized` - non-normalized `EO` programs
- `phi-normalized` - normalized `PHI` programs
- `eo-normalized` - normalized `EO` programs

The script will also produce reports in the `report` directory:

- `report.html` - `HTML` version of report hosted on our site
- `report.json` - `JSON` version of report that can be used for programmatic analysis of metrics
- `report.md` - `Markdown` version of report that's used in Job summaries in our GitHub Actions
