/**
 * The on-chain tx-order policy and the node's carriage reader, fed the same
 * vectors (N10b). The policy is the compiled `user_events/tx_order_v1` mint
 * from the built blueprint, run by the Lucid emulator's phase-2 evaluator;
 * the node side is `carriage.ts`, the code the forced-order projection and
 * hook read an order's carriage with.
 *
 * Each vector is a generated forced transaction, every non-empty field
 * carried inline, and a carriage vector: the SDK builder's own, or a forgery
 * of it (one byte of one preimage changed, a spare entry, a missing entry,
 * two entries swapped). The SDK builder makes the order; a forgery replaces
 * the vector in the mint redeemer it encodes, so the two transactions differ
 * in that vector alone. Both sides must agree: an honest vector is accepted
 * by the policy (the order lands), decoded from the landed redeemer and
 * hashed field by field back to the submitted transaction's preimages, and
 * ingested by the hook with the submitted bytes; a forged one is refused by
 * the policy's phase-2 run of the tx-order mint and by `carriage.ts` at the
 * named check.
 */
import { readFileSync } from "node:fs";

import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardForcedTxCanonical,
  encodeMidgardTxOutput,
  materializeMidgardForcedTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core";
import { deriveMidgardTxFieldPreimages } from "@al-ft/midgard-core/consensus-validation";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import { decodeTransaction, type FactStore } from "@al-ft/midgard-l1-follower";
import { SIM_ORIGIN } from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
  PROTOCOL_PARAMETERS_DEFAULT,
  type TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import fc from "fast-check";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import {
  carriageFieldPreimages,
  carriageVector,
  FORCED_ORDERS_TABLE,
  type ForcedOrderConfig,
  forcedOrderConfigFromContracts,
  forcedOrderProjection,
  reconstructTxOrderMaterial,
} from "../src/forced-orders/index.js";
import { AlwaysSucceedsContract } from "../src/services/always-succeeds.js";
import {
  db,
  forcedRows,
  ingestionHook,
  openNodeFollowerStore,
  UNCHANGED,
} from "./helpers/forced-orders-node-store.js";
import { realChain } from "./helpers/forced-orders-real-chain.js";
import { resetApplicationTables } from "./utils.js";

const K = 4;
const NETWORK = SELECTED_DEPLOYMENT_PROFILE.network;
const BLUEPRINT = SDK.parseFaultProofBlueprint(
  JSON.parse(
    readFileSync(
      new URL("../../../onchain/aiken/plutus.json", import.meta.url),
      "utf8",
    ),
  ),
);

const opened: FactStore[] = [];
beforeEach(async () => {
  await db(resetApplicationTables);
});
afterEach(async () => {
  vi.restoreAllMocks();
  await Promise.all(opened.splice(0).map((store) => store.close()));
});

type Harness = {
  lucid: LucidEvolution;
  emulator: Emulator;
  contracts: SDK.MidgardValidators;
  config: ForcedOrderConfig;
  creatorAddress: string;
};

/** Signs and submits `tx` on the emulator; its signed bytes. */
const submit = async (h: Harness, tx: TxSignBuilder): Promise<Buffer> => {
  const signed = await tx.sign.withWallet().complete();
  await signed.submit();
  h.emulator.awaitBlock(1);
  return Buffer.from(signed.toCBOR(), "hex");
};

/**
 * The emulator with the real tx-order validators over the placeholder set,
 * and a hub oracle naming them.
 */
const harness = async (): Promise<Harness> => {
  const creator = generateEmulatorAccount({ lovelace: 60_000_000_000n });
  const emulator = new Emulator([creator], {
    ...PROTOCOL_PARAMETERS_DEFAULT,
    maxCollateralInputs: 3,
  });
  const lucid = await Lucid(emulator, NETWORK);
  lucid.selectWallet.fromSeed(creator.seedPhrase);
  const placeholder = (await Effect.runPromise(
    AlwaysSucceedsContract.pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  )) as unknown as SDK.MidgardValidators;
  const contracts: SDK.MidgardValidators = {
    ...placeholder,
    ...SDK.buildTxOrderValidators({
      blueprint: BLUEPRINT,
      network: NETWORK,
      hubOraclePolicyId: placeholder.hubOracle.policyId,
    }),
  };
  const h: Harness = {
    lucid,
    emulator,
    contracts,
    config: forcedOrderConfigFromContracts(contracts),
    creatorAddress: await lucid.wallet().address(),
  };
  const [oneShot] = await lucid.wallet().getUtxos();
  await submit(
    h,
    await (
      await Effect.runPromise(
        SDK.incompleteHubOracleInitTxProgram(lucid, {
          hubOracleMintValidator: contracts.hubOracle,
          validators: contracts,
          oneShotNonceUTxO: oneShot!,
        }),
      )
    ).complete({ localUPLCEval: true }),
  );
  return h;
};

/** A generated forced transaction: outputs with small inline datums, and required signers. */
type TxShape = Readonly<{
  outputs: readonly Readonly<{
    fill: number;
    lovelace: bigint;
    datum: number;
  }>[];
  signers: readonly number[];
}>;

const shape: fc.Arbitrary<TxShape> = fc.record({
  outputs: fc.array(
    fc.record({
      fill: fc.integer({ min: 1, max: 250 }),
      lovelace: fc.bigInt({ min: 1_000_000n, max: 50_000_000n }),
      datum: fc.integer({ min: 1, max: 200 }),
    }),
    { minLength: 1, maxLength: 3 },
  ),
  signers: fc.uniqueArray(fc.integer({ min: 1, max: 250 }), {
    minLength: 1,
    maxLength: 3,
  }),
});

const transaction = (tx: TxShape): Buffer =>
  encodeMidgardForcedTxCanonical(
    materializeMidgardForcedTxFromCanonical({
      version: MIDGARD_NATIVE_TX_VERSION,
      body: {
        spendInputsPreimageCbor: EMPTY_CBOR_LIST,
        referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
        outputsPreimageCbor: encodeCbor(
          tx.outputs.map((output) =>
            encodeMidgardTxOutput({
              address: Buffer.concat([
                Buffer.from([0x60]),
                Buffer.alloc(28, output.fill),
              ]),
              value: { lovelace: output.lovelace, assets: new Map() },
              datum: {
                kind: "inline",
                cbor: Buffer.from(
                  aikenSerialisedPlutusDataCborPreservingMapOrder(
                    encodeCbor(
                      Buffer.alloc(output.datum, output.fill),
                    ).toString("hex"),
                  ),
                  "hex",
                ),
              },
            }),
          ),
        ),
        fee: 0n,
        validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
        validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
        requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
        requiredSignersPreimageCbor: encodeCbor(
          [...tx.signers].sort((a, b) => a - b).map((n) => Buffer.alloc(28, n)),
        ),
        mintPreimageCbor: EMPTY_CBOR_LIST,
        scriptIntegrityHash: EMPTY_NULL_ROOT,
        auxiliaryDataHash: EMPTY_NULL_ROOT,
        networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
      },
      witnessSet: {
        addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
        scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
        redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      },
    }),
  );

type Carriage = SDK.TxOrderMintRedeemer["material_carriage"];

const inlineBytes = (entry: Carriage[number]): string => {
  if (!("Inline" in entry)) throw new Error("expected inline carriage");
  return entry.Inline.preimage;
};

/** The forgeries, each with the check `carriage.ts` refuses it at. */
const FORGERIES: readonly Readonly<{
  name: string;
  forge: (carriage: Carriage) => Carriage;
  refusal: RegExp;
}>[] = [
  {
    name: "one byte of one preimage changed",
    forge: (carriage) =>
      carriage.map((entry, index) => {
        if (index !== carriage.length - 1) return entry;
        const bytes = Buffer.from(inlineBytes(entry), "hex");
        bytes[bytes.length - 1] ^= 0x01;
        return { Inline: { preimage: bytes.toString("hex") } };
      }),
    refusal: /field preimage does not match the committed field hash/u,
  },
  {
    name: "a spare entry",
    forge: (carriage) => [...carriage, carriage[0]!],
    refusal: /§8 carriage entries more than its commitments name/u,
  },
  {
    name: "a missing entry",
    forge: (carriage) => carriage.slice(0, -1),
    refusal: /with no §8 carriage for it/u,
  },
  {
    name: "two entries swapped",
    forge: (carriage) => [carriage[1]!, carriage[0]!, ...carriage.slice(2)],
    refusal: /field preimage does not match the committed field hash/u,
  },
];

/**
 * Builds the order for `submitted` with the SDK builder; `forge` replaces
 * the carriage vector of the mint redeemer it encodes. Returns the build's
 * outcome and the last redeemer the builder encoded.
 */
const buildOrder = async (
  h: Harness,
  submitted: Buffer,
  forge?: (carriage: Carriage) => Carriage,
) => {
  const nonce = [...(await h.lucid.wallet().getUtxos())]
    .filter((u) => u.datum == null)
    .sort((a, b) => Number(b.assets.lovelace - a.assets.lovelace))[0]!;
  const refundAddress = await Effect.runPromise(
    SDK.addressDataFromBech32(h.creatorAddress),
  );
  if (
    refundAddress.stakeCredential !== null &&
    "Pointer" in refundAddress.stakeCredential
  )
    throw new Error("the emulator creator must not hold a pointer address");
  const encode = Data.to.bind(Data);
  let redeemer: Buffer | undefined;
  const to = vi.spyOn(Data, "to").mockImplementation(((
    value: unknown,
    schema: unknown,
    options: unknown,
  ) => {
    if (schema !== SDK.TxOrderMintRedeemer)
      return (encode as (...args: unknown[]) => string)(value, schema, options);
    const honest = value as SDK.TxOrderMintRedeemer;
    const cbor = encode(
      forge === undefined
        ? honest
        : { ...honest, material_carriage: forge(honest.material_carriage) },
      SDK.TxOrderMintRedeemer,
    );
    redeemer = Buffer.from(cbor, "hex");
    return cbor;
  }) as typeof Data.to);
  const clock = vi
    .spyOn(Date, "now")
    .mockImplementation(() => h.emulator.now());
  try {
    const built = await Effect.runPromise(
      Effect.either(
        SDK.buildUnsignedTxOrderTxWithMetadataProgram(h.lucid, h.contracts, {
          nonceInput: nonce,
          submittedTxCbor: submitted.toString("hex"),
          carriageReferenceInputs: [],
          fieldPreimageCertificatePolicyId:
            h.contracts.fieldPreimageCertificate.policyId,
          refundAddress: {
            ...refundAddress,
            stakeCredential: refundAddress.stakeCredential,
          },
          lovelace: 3_000_000n,
        }),
      ),
    );
    if (redeemer === undefined)
      throw new Error("the builder encoded no tx-order mint redeemer");
    return { built, redeemer };
  } finally {
    clock.mockRestore();
    to.mockRestore();
  }
};

/** What `carriage.ts` makes of an inline-only redeemer for `submitted`. */
const nodeRead = (submitted: Buffer, redeemer: Buffer) => {
  const material = SDK.deriveTxOrderMaterial({
    submittedTxCbor: submitted,
    owner: Buffer.alloc(28),
  });
  const payload = {
    tx_id: material.transactionId,
    transaction_commitment: material.transactionCommitment,
    submitted_source: material.submitted_source,
  };
  const preimages = carriageFieldPreimages({
    payload,
    carriage: carriageVector(redeemer),
    referenceInputs: [],
    datumOf: () => null,
  });
  return {
    preimages,
    rebuilt: reconstructTxOrderMaterial({ payload, fieldPreimages: preimages }),
  };
};

const VECTORS = fc.sample(shape, { seed: 0x10b, numRuns: 3 });

describe("tx-order policy and carriage.ts agree (N10b)", () => {
  it.each(VECTORS.map((tx, index) => [index, tx] as const))(
    "honest vector %i: the policy accepts it, the order lands, and the node reads and ingests the same transaction",
    async (_index, tx) => {
      const h = await harness();
      const submitted = transaction(tx);
      const fields = deriveMidgardTxFieldPreimages(submitted, "forced").map(
        (field) => field.preimageCbor,
      );
      const { built, redeemer } = await buildOrder(h, submitted);
      if (built._tag === "Left") throw built.left;
      // The node side, from the redeemer the policy ran: every field hashed
      // back to the submitted transaction's preimage.
      const read = nodeRead(submitted, redeemer);
      expect(read.preimages).toEqual(fields);
      expect(read.rebuilt).toEqual(submitted);
      const order = await submit(h, built.right.tx);
      const store = await openNodeFollowerStore(
        [forcedOrderProjection(h.config)],
        K,
      );
      opened.push(store);
      expect((await store.initialize(SIM_ORIGIN)).kind).toBe("initialized");
      await realChain(store).forward([order]);
      // The follower read that redeemer off the landed transaction at the
      // policy's pointer, and resolved the carriage from it in the block.
      const landed = await store.transaction("read", (t) =>
        t.query(`SELECT status, mint_redeemer FROM ${FORCED_ORDERS_TABLE}`),
      );
      expect(landed).toHaveLength(1);
      expect(landed[0]!.status).toBe("resolved");
      expect(Buffer.from(landed[0]!.mint_redeemer as Buffer)).toEqual(redeemer);
      const { hook } = ingestionHook(store, h.config);
      expect(await hook(UNCHANGED)).toBeUndefined();
      const rows = await forcedRows();
      expect(rows).toHaveLength(1);
      expect(Buffer.from(rows[0]!.native_tx_cbor)).toEqual(submitted);
      expect(Buffer.from(rows[0]!.tx_order_l1_tx_hash)).toEqual(
        decodeTransaction(order).hash,
      );
    },
  );

  it.each(FORGERIES.map((f) => [f.name, f] as const))(
    "forged vector (%s): the policy refuses the tx-order mint and carriage.ts refuses it at its check",
    async (_name, forgery) => {
      const h = await harness();
      const submitted = transaction(VECTORS[0]!);
      const { built, redeemer } = await buildOrder(h, submitted, forgery.forge);
      expect(built._tag).toBe("Left");
      if (built._tag === "Left") {
        const text = String(built.left.message);
        // The order transaction runs one mint, the tx-order policy's: the
        // vector is all that differs from the honest build, and the policy
        // refuses it in phase 2.
        expect(text).toMatch(/failed script execution\s+Mint\[0\]/u);
      }
      expect(() => nodeRead(submitted, redeemer)).toThrow(forgery.refusal);
    },
  );
});
