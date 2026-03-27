# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0] - 2026-03-27

### Added
- Type-safe HTTP API routing using Elli with automatic OpenAPI documentation generation
- Integration with [Spectra](https://github.com/andreashasse/spectra) for Erlang type-based request/response validation
- URL routing with path parameter extraction (e.g. `<<"/api/users/{userId}">>`)
- Support for multiple HTTP status codes per endpoint via union types in function specs
- Request and response body validation and encoding (JSON and text/plain)
- Request header validation against declared function specs
- Swagger UI served at `/api-docs`
- Redoc UI served at `/redoc`
- OpenAPI spec stored in `persistent_term` for fast in-memory access
