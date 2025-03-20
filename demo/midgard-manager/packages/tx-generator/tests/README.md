# Transaction Generator Tests

This directory contains tests for the transaction generator package. The tests use Vitest as the test runner.

## Test Structure

- `setup.ts`: Contains setup code for the tests, including mock Lucid instances and test utilities
- `generate.test.ts`: Tests for the transaction generation scheduler functionality
- `node-client.test.ts`: Tests for the Midgard node client interactions
- `generators.test.ts`: Tests for the core transaction generator functions
- `utils.test.ts`: Tests for utility functions used throughout the package

## Current Test Strategy

The tests focus specifically on the transaction generator functionality:

1. **Core Transaction Generation**: Tests the basic transaction generation functionality with mocked dependencies
2. **Scheduler Functionality**: Tests the start, stop, and status operations of the generator
3. **Different Transaction Types**: Tests both one-to-one and multi-output transaction types
4. **Node Client**: Tests the node client for API communication
5. **Error Handling**: Tests the error handling capabilities of the system
6. **Utility Functions**: Tests the common utility functions used throughout the package

Wallet management functionality is not tested here as it belongs to the CLI package.

## Running Tests

To run the tests, use the following command from the root of the tx-generator package:

```bash
pnpm test
```

Or to run tests in watch mode (for development):

```bash
pnpm test:watch
```

## Test Environment

The tests run in a test environment with the following configuration:

- Network: Testnet
- Debug mode: Enabled
- Config path: ./temp-test-config

These settings are defined in the `.env.test` file in this directory.

## Mocking Strategy

The tests use extensive mocking to avoid external dependencies:

1. **Lucid**: Mocked to return predetermined UTxOs and addresses
2. **Node Client**: Mocked to simulate transaction submission without actual network calls
3. **Transaction Generation**: Mocked to return predetermined transaction objects
4. **CML Functions**: Mocked to provide consistent cryptographic operations

This allows for fast, reliable tests that can run in any environment.

## Adding New Tests

When adding new tests:

1. Follow the existing patterns in the test files
2. Use the `txTest` function from `setup.ts` to get access to the test context
3. Mock external dependencies where possible to keep tests focused and fast
4. Focus on testing the public API of the transaction generator package
