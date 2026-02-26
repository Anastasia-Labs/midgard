import { beforeEach, describe, expect, it, vi } from 'vitest';
import { CML } from '@lucid-evolution/lucid';

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

  it('should treat non-404 http responses as reachable node', async () => {
    mockFetch.mockResolvedValueOnce({
      status: 500,
    });

    const client = new MidgardNodeClient({
      baseUrl: 'http://localhost:3000',
      retryAttempts: 1,
      retryDelay: 10,
    });

    const isAvailable = await client.isAvailable();
    expect(isAvailable).toBe(true);
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

    const result = await client.submitTransaction('test_cbor_hex');

    // Check the result
    expect(result).toEqual({ txId: 'test_tx_id' });

    // Verify fetch was called with the correct arguments (second call)
    expect(mockFetch).toHaveBeenNthCalledWith(
      2,
      'http://localhost:3000/submit',
      expect.objectContaining({
        method: 'POST',
        headers: expect.objectContaining({
          'Content-Type': 'application/json',
        }),
        body: JSON.stringify({ tx_cbor: 'test_cbor_hex' }),
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

    const result = await client.submitTransaction('test_cbor_hex');
    expect(result).toEqual(
      expect.objectContaining({
        status: 'ERROR',
      })
    );
    expect(String((result as { error?: unknown }).error)).toContain('error');
  });

  it('should decode spendable utxos from node response', async () => {
    const txHash = CML.TransactionHash.from_hex('11'.repeat(32));
    const outRef = CML.TransactionInput.new(txHash, 0n);
    const output = CML.TransactionOutput.new(
      CML.Address.from_bech32(
        'addr_test1qzyem8ex0v9v76q0u52x3t2xmj5rkhjd9rsd44kx3klsut4qga2669x30zsng46mhfrrk4ngylfnnlda7rkfvxq5fywqvurkrs'
      ),
      CML.Value.from_coin(1_500_000n)
    );

    mockFetch.mockResolvedValueOnce({
      ok: true,
      status: 200,
      json: async () => ({
        utxos: [
          {
            outref: Buffer.from(outRef.to_cbor_bytes()).toString('hex'),
            value: Buffer.from(output.to_cbor_bytes()).toString('hex'),
          },
        ],
      }),
    });

    const client = new MidgardNodeClient({
      baseUrl: 'http://localhost:3000',
      retryAttempts: 1,
      retryDelay: 10,
    });

    const utxos = await client.getSpendableUtxos(
      'addr_test1qzyem8ex0v9v76q0u52x3t2xmj5rkhjd9rsd44kx3klsut4qga2669x30zsng46mhfrrk4ngylfnnlda7rkfvxq5fywqvurkrs'
    );
    expect(utxos).toHaveLength(1);
    expect(utxos[0].txHash).toBe('11'.repeat(32));
    expect(utxos[0].outputIndex).toBe(0);
    expect(utxos[0].assets.lovelace).toBe(1_500_000n);
  });
});
