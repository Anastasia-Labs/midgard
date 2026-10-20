import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  assertWatcherOrphanCancel,
  signWatcherOrphanCancel,
  WATCHER_ORPHAN_CANCEL_MAX_FEE_LOVELACE,
  WATCHER_ORPHAN_CANCEL_VALIDITY_MS,
  type WatcherOrphanCancelClause,
  WatcherOrphanCancelRefusedError,
} from "../../src/funding/prover-funding-orphan-cancel.transaction.js";
import { key, walletAddress } from "../support/fault-proof-funding-fixture.js";
import { cancelWallet } from "./orphan-cancel-fixture.js";

const NOW = 1_000_000_000;
const LEASED = `${"aa".repeat(32)}#0`;
const UNLEASED = `${"bb".repeat(32)}#1`;
const TTL = (NOW + WATCHER_ORPHAN_CANCEL_VALIDITY_MS) / 1000;

const inputList = (...outRefs: string[]) => {
  const inputs = CML.TransactionInputList.new();
  for (const outRef of outRefs) {
    const [hash, index] = outRef.split("#");
    inputs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(hash!),
        BigInt(index!),
      ),
    );
  }
  return inputs;
};

const coins = (lovelace: bigint) =>
  CML.Value.new(lovelace, CML.MultiAsset.new());

const own = CML.Address.from_bech32(walletAddress);
const otherKey = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x45))
  .to_public()
  .hash();
const other = CML.EnterpriseAddress.new(
  0,
  CML.Credential.new_pub_key(otherKey),
).to_address();

const outputList = (
  ...outputs: CML.TransactionOutput[]
): CML.TransactionOutputList => {
  const list = CML.TransactionOutputList.new();
  for (const output of outputs) list.add(output);
  return list;
};

type Parts = {
  body: CML.TransactionBody;
  witnesses: CML.TransactionWitnessSet;
  valid: boolean;
  auxiliary?: CML.AuxiliaryData;
};

/** A plain return of the leased coin to the wallet, as `change` alters it. */
const cancel = (change: (parts: Parts) => void = () => undefined) => {
  const parts: Parts = {
    body: CML.TransactionBody.new(
      inputList(LEASED),
      outputList(CML.TransactionOutput.new(own, coins(4_800_000n))),
      200_000n,
    ),
    witnesses: CML.TransactionWitnessSet.new(),
    valid: true,
  };
  parts.body.set_ttl(BigInt(TTL));
  change(parts);
  return CML.Transaction.new(
    parts.body,
    parts.witnesses,
    parts.valid,
    parts.auxiliary,
  );
};

const bounds = {
  leased: new Set([LEASED]),
  address: walletAddress,
  latestValidTo: NOW + WATCHER_ORPHAN_CANCEL_VALIDITY_MS,
  timeOf: (slot: number) => slot * 1000,
};

const refusal = (transaction: CML.Transaction) => {
  try {
    assertWatcherOrphanCancel(transaction, bounds);
  } catch (error) {
    if (error instanceof WatcherOrphanCancelRefusedError) return error.clause;
    throw error;
  }
  return null;
};

const datum = () => CML.PlutusData.new_integer(CML.BigInteger.from_str("1"));
const nativeScript = () => CML.NativeScript.new_script_pubkey(otherKey);

const VIOLATIONS: readonly (readonly [
  WatcherOrphanCancelClause,
  string,
  (parts: Parts) => void,
])[] = [
  [
    "inputs",
    "an out-ref the orphan does not lease",
    (parts) => {
      parts.body = CML.TransactionBody.new(
        inputList(LEASED, UNLEASED),
        parts.body.outputs(),
        parts.body.fee(),
      );
      parts.body.set_ttl(BigInt(TTL));
    },
  ],
  [
    "inputs",
    "no input at all",
    (parts) => {
      parts.body = CML.TransactionBody.new(
        inputList(),
        parts.body.outputs(),
        parts.body.fee(),
      );
      parts.body.set_ttl(BigInt(TTL));
    },
  ],
  [
    "output",
    "an output to another address",
    (parts) => {
      parts.body = CML.TransactionBody.new(
        inputList(LEASED),
        outputList(CML.TransactionOutput.new(other, coins(4_800_000n))),
        parts.body.fee(),
      );
      parts.body.set_ttl(BigInt(TTL));
    },
  ],
  [
    "output",
    "a second output",
    (parts) => {
      parts.body = CML.TransactionBody.new(
        inputList(LEASED),
        outputList(
          CML.TransactionOutput.new(own, coins(2_400_000n)),
          CML.TransactionOutput.new(own, coins(2_400_000n)),
        ),
        parts.body.fee(),
      );
      parts.body.set_ttl(BigInt(TTL));
    },
  ],
  [
    "mint",
    "a mint",
    ({ body }) => {
      const assets = CML.MapAssetNameToNonZeroInt64.new();
      assets.insert(CML.AssetName.from_str("x"), 1n);
      const mint = CML.Mint.new();
      mint.insert_assets(CML.ScriptHash.from_hex("cc".repeat(28)), assets);
      body.set_mint(mint);
    },
  ],
  [
    "certificates",
    "a certificate",
    ({ body }) => {
      const certs = CML.CertificateList.new();
      certs.add(
        CML.Certificate.new_stake_registration(
          CML.Credential.new_pub_key(otherKey),
        ),
      );
      body.set_certs(certs);
    },
  ],
  [
    "governance",
    "a treasury donation",
    ({ body }) => {
      body.set_donation(1n);
    },
  ],
  [
    "governance",
    "a treasury value",
    ({ body }) => {
      body.set_current_treasury_value(1n);
    },
  ],
  [
    "withdrawals",
    "a withdrawal",
    ({ body }) => {
      const withdrawals = CML.MapRewardAccountToCoin.new();
      withdrawals.insert(
        CML.RewardAddress.new(0, CML.Credential.new_pub_key(otherKey)),
        1n,
      );
      body.set_withdrawals(withdrawals);
    },
  ],
  [
    "scripts",
    "a phase-two failure flag",
    (parts) => {
      parts.valid = false;
    },
  ],
  [
    "scripts",
    "a script data hash",
    ({ body }) => {
      body.set_script_data_hash(CML.ScriptDataHash.from_hex("dd".repeat(32)));
    },
  ],
  [
    "scripts",
    "a required signer",
    ({ body }) => {
      const signers = CML.Ed25519KeyHashList.new();
      signers.add(otherKey);
      body.set_required_signers(signers);
    },
  ],
  [
    "scripts",
    "a datum on its output",
    (parts) => {
      parts.body = CML.TransactionBody.new(
        inputList(LEASED),
        outputList(
          CML.TransactionOutput.new(
            own,
            coins(4_800_000n),
            CML.DatumOption.new_datum(datum()),
          ),
        ),
        parts.body.fee(),
      );
      parts.body.set_ttl(BigInt(TTL));
    },
  ],
  [
    "scripts",
    "a reference script on its output",
    (parts) => {
      parts.body = CML.TransactionBody.new(
        inputList(LEASED),
        outputList(
          CML.TransactionOutput.new(
            own,
            coins(4_800_000n),
            undefined,
            CML.Script.new_native(nativeScript()),
          ),
        ),
        parts.body.fee(),
      );
      parts.body.set_ttl(BigInt(TTL));
    },
  ],
  [
    "scripts",
    "a native script witness",
    ({ witnesses }) => {
      const scripts = CML.NativeScriptList.new();
      scripts.add(nativeScript());
      witnesses.set_native_scripts(scripts);
    },
  ],
  [
    "scripts",
    "a datum witness",
    ({ witnesses }) => {
      const datums = CML.PlutusDataList.new();
      datums.add(datum());
      witnesses.set_plutus_datums(datums);
    },
  ],
  [
    "metadata",
    "an auxiliary data hash",
    ({ body }) => {
      body.set_auxiliary_data_hash(
        CML.AuxiliaryDataHash.from_hex("ee".repeat(32)),
      );
    },
  ],
  [
    "metadata",
    "auxiliary data",
    (parts) => {
      parts.auxiliary = CML.AuxiliaryData.new_shelley(CML.Metadata.new());
    },
  ],
  [
    "reference_inputs",
    "a reference input",
    ({ body }) => {
      body.set_reference_inputs(inputList(LEASED));
    },
  ],
  [
    "collateral",
    "a collateral input",
    ({ body }) => {
      body.set_collateral_inputs(inputList(LEASED));
    },
  ],
  [
    "collateral",
    "a total collateral",
    ({ body }) => {
      body.set_total_collateral(1n);
    },
  ],
  [
    "fee",
    "a fee above the maximum",
    (parts) => {
      parts.body = CML.TransactionBody.new(
        inputList(LEASED),
        parts.body.outputs(),
        WATCHER_ORPHAN_CANCEL_MAX_FEE_LOVELACE + 1n,
      );
      parts.body.set_ttl(BigInt(TTL));
    },
  ],
  [
    "validity",
    "no expiry",
    (parts) => {
      parts.body = CML.TransactionBody.new(
        inputList(LEASED),
        parts.body.outputs(),
        parts.body.fee(),
      );
    },
  ],
  [
    "validity",
    "an expiry past the hour",
    ({ body }) => {
      body.set_ttl(BigInt(TTL + 1));
    },
  ],
];

describe("the transaction an orphan cancel signs", () => {
  it("is a plain return of leased coins to the wallet's own address", () => {
    expect(refusal(cancel())).toBeNull();
    expect(
      refusal(
        cancel(({ body }) => {
          body.set_ttl(BigInt(TTL));
        }),
      ),
    ).toBeNull();
  });

  it.each(
    VIOLATIONS.map(([clause, what, change]) => ({ clause, what, change })),
  )("is refused with $what ($clause)", ({ clause, change }) => {
    expect(refusal(cancel(change))).toBe(clause);
  });
});

describe("signing an orphan cancel", () => {
  const funding = {
    outRef: LEASED,
    role: "funding" as const,
    lovelace: "5000000",
    assets: [],
  };
  const fees = { minFeeA: 44n, minFeeB: 155_381n, coinsPerUtxoByte: 4_310n };

  it("checks the final body it signs, and signs one that passes with the wallet's key", async () => {
    const wallet = cancelWallet();
    const signed = CML.Transaction.from_cbor_hex(
      await signWatcherOrphanCancel({
        inputs: [funding],
        wallet: wallet.wallet,
        fees,
        now: NOW,
      }),
    );
    expect(refusal(signed)).toBeNull();
    expect(wallet.signed).toHaveLength(1);
    const keys = signed.witness_set().vkeywitnesses()!;
    expect(keys.len()).toBe(1);
    expect(keys.get(0).vkey().to_bech32()).toBe(key.to_public().to_bech32());
  });

  it("refuses to sign a cancel whose fee would pass the maximum", async () => {
    const wallet = cancelWallet();
    await expect(
      signWatcherOrphanCancel({
        inputs: [funding],
        wallet: wallet.wallet,
        fees: { ...fees, minFeeA: 10_000n },
        now: NOW,
      }),
    ).rejects.toMatchObject({ clause: "fee" });
    expect(wallet.signed).toEqual([]);
  });

  it("refuses to sign a cancel valid past the hour", async () => {
    const wallet = cancelWallet();
    const session = await wallet.wallet.open();
    await expect(
      signWatcherOrphanCancel({
        inputs: [funding],
        wallet: {
          ...wallet.wallet,
          open: async () => ({
            ...session,
            timeOf: (slot) =>
              slot * 1000 + 2 * WATCHER_ORPHAN_CANCEL_VALIDITY_MS,
          }),
        },
        fees,
        now: NOW,
      }),
    ).rejects.toMatchObject({ clause: "validity" });
    expect(wallet.signed).toEqual([]);
  });

  it("signs nothing when the funding cannot cover its fee and minimum output", async () => {
    const wallet = cancelWallet();
    await expect(
      signWatcherOrphanCancel({
        inputs: [{ ...funding, lovelace: "300000" }],
        wallet: wallet.wallet,
        fees,
        now: NOW,
      }),
    ).rejects.toThrow("cannot cover its fee and minimum output");
    expect(wallet.signed).toEqual([]);
  });
});
