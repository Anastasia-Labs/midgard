import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import type { L1NodeTransport } from "@al-ft/l1-node-transport";
import {
  CML,
  credentialToAddress,
  Lucid,
  type UTxO,
} from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { blake2b256 } from "../../src/codec.js";
import type { BlockSummary, FactStore, TrackedSet } from "../../src/index.js";
import {
  L1AwaitTxTimeoutError,
  L1CarriagePendingError,
  L1FollowerProvider,
  L1LocalEvaluationOnlyError,
  L1ProviderRequestError,
  L1ProviderScopeError,
  L1ProviderTransientError,
  L1SubmitRejectedError,
  L1UnitLookupError,
} from "../../src/provider/index.js";
import { testDatabases } from "../support/postgres.js";
import {
  built,
  DEVNET_LEDGER,
  fakeLedgerTransport,
  ledgerEntry,
  type LedgerOptions,
  providerAdapters,
  witnessWithDatums,
} from "../support/provider.js";
import { fill, ORIGIN, tx } from "../support/small-chain.js";

const databases = testDatabases();
const scratch = mkdtempSync(join(tmpdir(), "l1-follower-provider-"));

const hex = (bytes: Buffer): string => bytes.toString("hex");
const SCRIPT = "bb".repeat(28);
const POLICY = "cc".repeat(28);
const unit = (name: string): string => `${POLICY}${name}`;
const DATUM_1 = Buffer.from("d8799f4401020304ff", "hex");
const DATUM_2 = Buffer.from("d87a9f01ff", "hex");
const H1 = hex(blake2b256(DATUM_1));
const H2 = hex(blake2b256(DATUM_2));

const walletKey = CML.PrivateKey.generate_ed25519();
const wallet = credentialToAddress("Custom", {
  type: "Key",
  hash: walletKey.to_public().hash().to_hex(),
});
const scriptAddress = credentialToAddress("Custom", {
  type: "Script",
  hash: SCRIPT,
});
const untracked = credentialToAddress("Custom", {
  type: "Key",
  hash: "ee".repeat(28),
});

const TXA = fill(0xa1);
const TXB = fill(0xa2);
const TXC = fill(0xa3);
const SEED = fill(0x5e);
const ada = (n: number) => ({ lovelace: BigInt(n) * 1_000_000n });

// b1 creates wallet, script and untracked outputs; b2 spends TXA#4 (whose
// datum preimage only the spender carries); b3 is a phase-2 failure that
// consumes TXA#5 as collateral and returns 2 ADA to the wallet.
const A = [
  built({ txHash: TXA, index: 0 }, { address: wallet, assets: ada(50) }),
  built(
    { txHash: TXA, index: 1 },
    {
      address: scriptAddress,
      assets: { ...ada(2), [unit("01")]: 1n },
      datumHash: H1,
    },
  ),
  built(
    { txHash: TXA, index: 2 },
    {
      address: scriptAddress,
      assets: ada(20),
      inlineDatum: "d87980",
      scriptRef: { type: "PlutusV3", script: "4e4d01000033222220051200120011" },
    },
  ),
  built({ txHash: TXA, index: 3 }, { address: untracked, assets: ada(1) }),
  built(
    { txHash: TXA, index: 4 },
    { address: scriptAddress, assets: ada(2), datumHash: H2 },
  ),
  built({ txHash: TXA, index: 5 }, { address: wallet, assets: ada(3) }),
  built(
    { txHash: TXA, index: 6 },
    { address: scriptAddress, assets: { ...ada(2), [unit("03")]: 1n } },
  ),
];
const B = [
  built(
    { txHash: TXB, index: 0 },
    { address: scriptAddress, assets: { ...ada(2), [unit("02")]: 1n } },
  ),
  built(
    { txHash: TXB, index: 1 },
    { address: scriptAddress, assets: { ...ada(2), [unit("03")]: 1n } },
  ),
];
const C_RETURN = built(
  { txHash: TXC, index: 0 },
  { address: wallet, assets: ada(2) },
);
const SEEDED = built(
  { txHash: SEED, index: 0 },
  { address: wallet, assets: ada(7) },
);
const LEDGER_UNTRACKED = built(
  { txHash: fill(0x71), index: 0 },
  { address: untracked, assets: ada(4) },
);
const LEDGER_TRACKED_UNSEEN = built(
  { txHash: fill(0x72), index: 0 },
  { address: wallet, assets: ada(9) },
);

const blocks = (): BlockSummary[] => [
  {
    point: { slot: 101, hash: fill(0xc1) },
    height: 51,
    parentHash: ORIGIN.point.hash,
    txs: [
      tx(TXA, {
        inputs: [{ txHash: fill(0x01), index: 0 }],
        outputs: A.map((output) => output.summary),
        witnessCbor: witnessWithDatums([DATUM_1]),
      }),
    ],
  },
  {
    point: { slot: 103, hash: fill(0xc2) },
    height: 52,
    parentHash: fill(0xc1),
    txs: [
      tx(TXB, {
        inputs: [{ txHash: TXA, index: 4 }],
        outputs: B.map((output) => output.summary),
        witnessCbor: witnessWithDatums([DATUM_2], true),
      }),
    ],
  },
  {
    point: { slot: 105, hash: fill(0xc3) },
    height: 53,
    parentHash: fill(0xc2),
    txs: [
      tx(TXC, {
        isValid: false,
        inputs: [{ txHash: fill(0x02), index: 0 }],
        collaterals: [{ txHash: TXA, index: 5 }],
        collateralReturn: C_RETURN.summary,
      }),
    ],
  },
];

const TRACKED: TrackedSet = {
  addresses: new Set([hex(A[0]!.summary.address)]),
  paymentCredentials: new Set([SCRIPT]),
  policies: new Set(),
};

const MEMPOOL_TX = "77".repeat(32);
const ledger = (extra: LedgerOptions = {}): LedgerOptions => ({
  answers: DEVNET_LEDGER.lsq,
  utxos: [LEDGER_UNTRACKED, LEDGER_TRACKED_UNSEEN, A[3]!].map(ledgerEntry),
  mempool: [MEMPOOL_TX],
  submit: "accept",
  ...extra,
});

const REJECTION = Buffer.from("8182068182028200a0", "hex");
/** A minimal transaction: [body {0: [], 1: [], 2: 0}, {}, true, null]. */
const BODY = Buffer.from("a300d9010280018002 00".replace(/ /gu, ""), "hex");
const SMALL_TX = Buffer.concat([
  Buffer.from([0x84]),
  BODY,
  Buffer.from([0xa0, 0xf5, 0xf6]),
]);

let accepting: L1NodeTransport;
let rejecting: L1NodeTransport;
let refusing: L1NodeTransport;
beforeAll(async () => {
  accepting = await fakeLedgerTransport(scratch, ledger());
  rejecting = await fakeLedgerTransport(
    scratch,
    ledger({ submit: { rejection: hex(REJECTION) } }),
  );
  refusing = await fakeLedgerTransport(
    scratch,
    ledger({
      refuse: {
        protocol_params: "node_unavailable",
        utxo_by_address: "acquire_failed",
      },
    }),
  );
});
afterAll(async () => {
  await Promise.all([accepting, rejecting, refusing].map((t) => t?.close()));
  await databases.dropAll();
  rmSync(scratch, { recursive: true, force: true });
});

const refs = (utxos: readonly UTxO[]): string[] =>
  utxos.map((utxo) => `${utxo.txHash}#${utxo.outputIndex}`);
const ref = (output: { outRef: { txHash: Buffer; index: number } }): string =>
  `${hex(output.outRef.txHash)}#${output.outRef.index}`;

describe.each(providerAdapters(databases, scratch))(
  "L1 follower provider ($name)",
  (adapter) => {
    let store: FactStore;
    let provider: L1FollowerProvider;
    beforeAll(async () => {
      store = await adapter.open(TRACKED);
      expect(await store.start()).toMatchObject({ kind: "ready" });
      // Before the follower is initialized a tracked read is a transient refusal.
      await expect(
        new L1FollowerProvider({ store, transport: accepting }).getUtxos(
          wallet,
        ),
      ).rejects.toMatchObject({
        source: "follower",
        reason: "not_initialized",
      });
      expect(await store.initialize(ORIGIN)).toMatchObject({
        kind: "initialized",
      });
      expect(
        await store.insertSeedOutputs(ORIGIN.point, [
          { outRef: SEEDED.outRef, output: SEEDED.summary },
        ]),
      ).toMatchObject({ kind: "seeded" });
      for (const block of blocks())
        expect(await store.applyBlock(block)).toMatchObject({
          kind: "applied",
        });
      provider = new L1FollowerProvider({
        store,
        transport: accepting,
        awaitTxTimeoutMs: 400,
      });
    });
    afterAll(async () => await store.close());

    it("answers a tracked address from the facts, seed rows included and spent rows not", async () => {
      expect(refs(await provider.getUtxos(wallet)).sort()).toEqual(
        [A[0]!, SEEDED, C_RETURN].map(ref).sort(),
      );
      const scripts = await provider.getUtxos({ type: "Script", hash: SCRIPT });
      expect(refs(scripts).sort()).toEqual(
        [A[1]!, A[2]!, A[6]!, ...B].map(ref).sort(),
      );
      expect(await provider.getUtxos(scriptAddress)).toEqual(scripts);
    });

    it("answers an untracked address from the ledger and refuses an untracked credential", async () => {
      expect(refs(await provider.getUtxos(untracked)).sort()).toEqual(
        [ref(LEDGER_UNTRACKED), ref(A[3]!)].sort(),
      );
      await expect(
        provider.getUtxos({ type: "Key", hash: "ee".repeat(28) }),
      ).rejects.toBeInstanceOf(L1ProviderScopeError);
    });

    it("answers outrefs from the facts first and keeps the tracked scope the follower's", async () => {
      const answer = await provider.getUtxosByOutRef(
        [A[0]!, A[4]!, A[3]!, LEDGER_TRACKED_UNSEEN, A[0]!]
          .map((output) => ({
            txHash: hex(output.outRef.txHash),
            outputIndex: output.outRef.index,
          }))
          .concat([{ txHash: "99".repeat(32), outputIndex: 0 }]),
      );
      expect(refs(answer)).toEqual([ref(A[0]!), ref(A[3]!)]);
    });

    it("finds one unit holder, and refuses none or several", async () => {
      expect(refs([await provider.getUtxoByUnit(unit("01"))])).toEqual([
        ref(A[1]!),
      ]);
      expect(refs([await provider.getUtxoByUnit(unit("02"))])).toEqual([
        ref(B[0]!),
      ]);
      await expect(provider.getUtxoByUnit(unit("03"))).rejects.toMatchObject({
        constructor: L1UnitLookupError,
        found: 2,
      });
      await expect(provider.getUtxoByUnit(unit("04"))).rejects.toMatchObject({
        found: 0,
      });
      expect(
        refs(await provider.getUtxosWithUnit(scriptAddress, unit("03"))).sort(),
      ).toEqual([ref(A[6]!), ref(B[1]!)].sort());
      expect(
        refs(await provider.getUtxosWithPolicy(scriptAddress, POLICY)).length,
      ).toBe(4);
    });

    it("reads datum preimages from creator and spender witnesses, never an empty answer", async () => {
      expect(await provider.getDatum(H1)).toBe(hex(DATUM_1));
      expect(await provider.getDatum(H2)).toBe(hex(DATUM_2));
      await expect(provider.getDatum("dd".repeat(32))).rejects.toBeInstanceOf(
        L1CarriagePendingError,
      );
    });

    it("reports transaction status from the facts and the mempool", async () => {
      expect(await provider.getTransactionStatus(hex(TXA))).toEqual({
        status: "confirmed",
        txHash: hex(TXA),
        confirmation: {
          txHash: hex(TXA),
          slot: 101,
          blockHash: hex(fill(0xc1)),
          blockHeight: 51,
          confirmations: 3,
        },
      });
      expect(await provider.getTransactionStatus(hex(TXC))).toMatchObject({
        status: "failed",
      });
      expect(await provider.getTransactionStatus(MEMPOOL_TX)).toEqual({
        status: "pending",
        txHash: MEMPOOL_TX,
      });
      expect(
        await provider.getTransactionStatus("78".repeat(32)),
      ).toMatchObject({
        status: "not_found",
      });
    });

    it("awaits a tx until the facts hold it, and times out with what it last saw", async () => {
      expect(await provider.awaitTx(hex(TXB), 50)).toBe(true);
      await expect(provider.awaitTx(MEMPOOL_TX, 50)).rejects.toMatchObject({
        constructor: L1AwaitTxTimeoutError,
        lastSeen: "in_mempool",
      });
      await expect(provider.awaitTx("78".repeat(32), 50)).rejects.toMatchObject(
        {
          lastSeen: "absent",
        },
      );
    });

    it("submits through LocalTxSubmission; a rejection carries the ledger's bytes", async () => {
      const txHash = CML.hash_transaction(
        CML.TransactionBody.from_cbor_bytes(BODY),
      ).to_hex();
      expect(await provider.submitTx(hex(SMALL_TX))).toBe(txHash);
      const rejected = new L1FollowerProvider({ store, transport: rejecting });
      const error = await rejected
        .submitTx(hex(SMALL_TX))
        .catch((e: unknown) => e);
      expect(error).toBeInstanceOf(L1SubmitRejectedError);
      expect((error as L1SubmitRejectedError).txHash).toBe(txHash);
      expect(Buffer.from((error as L1SubmitRejectedError).rejection)).toEqual(
        REJECTION,
      );
      await expect(provider.submitTx("ff00")).rejects.toMatchObject({
        constructor: L1ProviderRequestError,
        code: "tx_undecodable",
      });
    });

    it("never evaluates remotely", async () => {
      await expect(provider.evaluateTx()).rejects.toBeInstanceOf(
        L1LocalEvaluationOnlyError,
      );
    });

    it("turns node, LSQ and store unavailability into typed transient errors", async () => {
      const refused = new L1FollowerProvider({ store, transport: refusing });
      await expect(refused.getProtocolParameters()).rejects.toMatchObject({
        constructor: L1ProviderTransientError,
        source: "transport",
        reason: "node_unavailable",
      });
      await expect(refused.getUtxos(untracked)).rejects.toMatchObject({
        source: "transport",
        reason: "acquire_failed",
      });
      const closed = await fakeLedgerTransport(scratch, ledger());
      await closed.close();
      await expect(
        new L1FollowerProvider({ store, transport: closed }).submitTx(
          hex(SMALL_TX),
        ),
      ).rejects.toMatchObject({
        constructor: L1ProviderTransientError,
        source: "transport",
      });
      const gone = await adapter.open(TRACKED);
      await gone.close();
      await expect(
        new L1FollowerProvider({ store: gone, transport: accepting }).getUtxos(
          wallet,
        ),
      ).rejects.toMatchObject({
        constructor: L1ProviderTransientError,
        source: "store",
      });
    });

    it("drives Lucid coin selection, local evaluation and submission with no other provider", async () => {
      const lucid = await Lucid(provider, "Custom", {
        slotConfig: await provider.slotConfig(),
      });
      lucid.selectWallet.fromPrivateKey(walletKey.to_bech32());
      const signed = await (
        await lucid
          .newTx()
          .pay.ToAddress(untracked, ada(52))
          .complete({ localUPLCEval: true })
      ).sign
        .withWallet()
        .complete();
      const body = signed.toTransaction().body();
      const spent: string[] = [];
      for (let index = 0; index < body.inputs().len(); index += 1) {
        const input = body.inputs().get(index);
        spent.push(`${input.transaction_id().to_hex()}#${input.index()}`);
      }
      // 52 ADA plus fee needs the 50 ADA output and at least one more wallet output.
      expect(spent).toContain(ref(A[0]!));
      expect(
        spent.every((input) =>
          [A[0]!, SEEDED, C_RETURN].map(ref).includes(input),
        ),
      ).toBe(true);
      expect(await signed.submit()).toBe(signed.toHash());
    });
  },
);
