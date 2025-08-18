import { Effect } from 'effect';
import { beforeEach, describe, expect, it, vi } from 'vitest';

import { MidgardNodeClient } from '../src/lib/client/node-client';

// Mock fetch
const mockFetch = vi.fn();
global.fetch = mockFetch;

describe('MidgardNodeClient', () => {
  // Reset mocks before each test
  beforeEach(() => {
    mockFetch.mockReset();
  });

  it('should check if the node is available', async () => {
    // Mock a 404 response (indicating the node is available but the tx not found)
    mockFetch.mockResolvedValueOnce({
      status: 404,
    });

    // Create a client instance
    const client = new MidgardNodeClient({
      baseUrl: 'http://localhost:3000',
      retryAttempts: 1,
      retryDelay: 10,
    });

    // Check availability
    const isAvailable = await client.isAvailable();

    // Verify result and fetch call - use a matcher for the second param to handle the AbortSignal
    expect(isAvailable).toBe(true);
    expect(mockFetch).toHaveBeenCalledWith(
      'http://localhost:3000/tx?tx_hash=0000000000000000000000000000000000000000000000000000000000000000',
      expect.objectContaining({
        signal: expect.any(AbortSignal),
      })
    );
  });

  it('should return false when node is not available', async () => {
    // Mock a network error
    mockFetch.mockRejectedValueOnce(new Error('Network error'));

    // Create a client instance
    const client = new MidgardNodeClient({
      baseUrl: 'http://localhost:3000',
      retryAttempts: 1,
      retryDelay: 10,
    });

    // Check availability
    const isAvailable = await client.isAvailable();

    // Verify result
    expect(isAvailable).toBe(false);
  });

  it('should submit a transaction correctly', async () => {
    // First mock the availability check (404 means node is up but tx not found)
    mockFetch
      .mockResolvedValueOnce({
        status: 404,
      })
      .mockResolvedValueOnce({
        ok: true,
        status: 200,
        json: async () => ({ txId: 'test_tx_id' }),
      });

    // Create a client instance
    const client = new MidgardNodeClient({
      baseUrl: 'http://localhost:3000',
      retryAttempts: 1,
      retryDelay: 10,
    });

    // Submit the transaction - the client returns an Effect
    const effectResult = client.submitTransaction('test_cbor_hex');

    // Run the effect to get the result
    const result = await Effect.runPromise(effectResult);

    // Check the result
    expect(result).toEqual({ txId: 'test_tx_id' });

    // Verify fetch was called with the correct arguments (second call)
    expect(mockFetch).toHaveBeenNthCalledWith(
      2,
      'http://localhost:3000/submit?tx_cbor=test_cbor_hex',
      expect.objectContaining({
        method: 'POST',
        headers: expect.objectContaining({
          'Content-Type': 'text/plain',
        }),
      })
    );
  });

  it('should handle network errors', async () => {
    // First mock the availability check (node is available)
    mockFetch
      .mockResolvedValueOnce({
        status: 404,
      })
      .mockRejectedValueOnce(new Error('Network error'));

    // Create a client instance
    const client = new MidgardNodeClient({
      baseUrl: 'http://localhost:3000',
      retryAttempts: 1,
      retryDelay: 10,
    });

    // Submit the transaction - this returns an Effect
    const effectResult = client.submitTransaction('test_cbor_hex');

    // Set up a flag to check if the error was caught
    let errorCaught = false;

    try {
      // Try to run the effect - this should fail
      await Effect.runPromise(effectResult);
    } catch (error) {
      // Just verify an error was thrown without checking its structure
      errorCaught = true;
    }

    // Assert that an error was caught
    expect(errorCaught).toBe(true);
  });
});
