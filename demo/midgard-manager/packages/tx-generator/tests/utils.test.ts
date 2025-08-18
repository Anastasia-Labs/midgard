import { CML } from '@lucid-evolution/lucid';
import { describe, expect, it, vi } from 'vitest';

import {
  getPrivateKeyCborHex,
  getPublicKeyHashFromPrivateKey,
  parseUnknownKeytoBech32PrivateKey,
  serializeAssets,
} from '../src/utils/common';

// Mock the CML functionality
vi.mock('@lucid-evolution/lucid', () => {
  return {
    CML: {
      PrivateKey: {
        from_bech32: vi.fn().mockReturnValue({
          to_public: vi.fn().mockReturnValue({
            hash: vi.fn().mockReturnValue({
              to_hex: vi.fn().mockReturnValue('test_public_key_hash'),
            }),
          }),
          to_raw_bytes: vi.fn().mockReturnValue(new Uint8Array([1, 2, 3, 4])),
        }),
        from_normal_bytes: vi.fn().mockReturnValue({
          to_bech32: vi.fn().mockReturnValue('test_bech32_private_key'),
        }),
      },
    },
    walletFromSeed: vi.fn().mockReturnValue({
      paymentKey: 'seed_generated_key',
    }),
    Data: {
      to: vi.fn().mockReturnValue('test_cbor_hex'),
    },
  };
});

describe('Utility Functions', () => {
  it('should serialize assets correctly', () => {
    const assets = {
      lovelace: 1000000n,
      'policy.tokenName': 5n,
    };

    const serialized = serializeAssets(assets);

    expect(serialized).toEqual({
      lovelace: '1000000',
      'policy.tokenName': '5',
    });
  });

  it('should parse string private key', () => {
    // This test is simplified because we've fully mocked the CML functionality
    const result = parseUnknownKeytoBech32PrivateKey('test_private_key');
    expect(result).toBe('test_bech32_private_key');
  });

  it('should parse seed phrase private key', () => {
    // Test with a mock seed phrase (with spaces)
    const result = parseUnknownKeytoBech32PrivateKey('word1 word2 word3');
    expect(result).toBe('seed_generated_key');
  });

  it('should get public key hash from private key', () => {
    const result = getPublicKeyHashFromPrivateKey('test_private_key');
    expect(result).toBe('test_public_key_hash');
  });

  it('should get CBOR hex from private key', () => {
    const result = getPrivateKeyCborHex('test_private_key');
    expect(result).toBe('test_cbor_hex');
  });

  it('should throw error for non-string private key', () => {
    expect(() => {
      parseUnknownKeytoBech32PrivateKey(123 as any);
    }).toThrow('Expected a string value for the private key');
  });
});
