# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

### Fixed

- `Complex.fromString` with space between operator and i (imaginary part of 1).

### Added

- `Complex.sqrt`.

### Tools

- Upgrade elm-test to 0.19.0-beta11.

## [1.0.5] - 2018-08-14

### Fixed

- `Complex.pow` when base is zero.
- argument given by `Complex.toPolar`.

## [1.0.4] - 2018-08-13

### Fixed

- Fixed `Complex.fromString` parsing inputs `"i"` and `"+i"`

## [1.0.3] - 2018-08-13

### Docs

- Improved formatting in docs.

## [1.0.2] - 2018-08-13

### Docs

- Improved formatting in docs.

## [1.0.1] - 2018-08-13

### Docs

- Updates to README.

## [1.0.0] - 2018-08-13

Initial version