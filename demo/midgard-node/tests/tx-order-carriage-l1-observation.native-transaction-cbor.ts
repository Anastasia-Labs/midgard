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
import * as SDK from "@al-ft/midgard-sdk";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid as makeLucid,
  type LucidEvolution,
  paymentCredentialOf,
  PROTOCOL_PARAMETERS_DEFAULT,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { AlwaysSucceedsContract } from "../src/services/always-succeeds.js";
import {
  type LocalL1,
  startLocalL1Observation,
} from "./helpers/local-l1-observation.js";

/**
 * #599: the node ingests a **material-bearing** forced order, at all three §8
 * carriage tiers, with the carriage read off L1 rather than handed to the
 * authenticator by a test.
 *
 * `tests/tx-order-material-chain.test.ts` covers the authenticator by feeding
 * it carriage directly, which is the right shape for testing a door. This file
 * covers the thing that file cannot: that a *source* exists, that it is the
 * node's own (Ogmios chain-sync + Kupo, no watcher), and that what it returns
 * opens the door for an order nobody handed us.
 *
 * **What is real here.** The order transaction is built on the Lucid emulator and
 * really submitted, so its bytes are a ledger's. Its mint redeemer is produced by
 * the SDK's own `txOrderMaterialCarriageVector` against the transaction's actual
 * reference-input set, its datum by the SDK's `TxOrderDatum`, and its carriage
 * by the SDK's publication and certification builders — no §8 value in the
 * observation is written by this file. The observation surfaces
 * (`helpers/local-l1-observation.ts`) implement Ogmios chain-sync over a real
 * WebSocket and Kupo over real HTTP, serving that transaction's own CBOR-derived
 * view, so the read under test performs every step it performs in production:
 * Kupo match → ancestor checkpoint → `findIntersection` → `nextBlock` forward →
 * mint-redeemer extraction → reference-input resolution.
 *
 * **What is not real, and why it cannot be here.** There is no local cardano-node
 * (`docker-compose.kupmios.yaml` is a docker deployment, not a test fixture),
 * so slots and header hashes are the harness's and the tx-order minting policy is
 * the always-succeeds placeholder every emulator suite in this package uses. The
 * mint's own §8.11 walk is therefore not re-run here — it is covered in Aiken —
 * and what is asserted is exactly this ticket's claim: the node can now *source*
 * the vector that walk authenticated.
 */

export const EMULATOR_PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  maxCollateralInputs: 3,
} as const;

/**
 * One 5 kB-datum output per fill byte, exactly as
 * `tx-order-material-chain.test.ts` sizes them: two outputs put field 2 inside
 * §8.3's `K` and four put it above, which is what makes the tier assertions below
 * §8.4's partition rather than this file's preference.
 */
export const nativeTransactionCbor = (outputFills: readonly number[]): Buffer =>
  encodeMidgardForcedTxCanonical(
    materializeMidgardForcedTxFromCanonical({
      version: MIDGARD_NATIVE_TX_VERSION,
      body: {
        spendInputsPreimageCbor: EMPTY_CBOR_LIST,
        referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
        outputsPreimageCbor: encodeCbor(
          outputFills.map((fill) =>
            encodeMidgardTxOutput({
              address: Buffer.concat([
                Buffer.from([0x60]),
                Buffer.alloc(28, fill),
              ]),
              value: { lovelace: 2_000_000n, assets: new Map() },
              datum: {
                kind: "inline",
                cbor: Buffer.from(
                  aikenSerialisedPlutusDataCborPreservingMapOrder(
                    encodeCbor(Buffer.alloc(5_000, fill)).toString("hex"),
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
        requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
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

export type Harness = {
  readonly lucid: LucidEvolution;
  readonly emulator: Emulator;
  readonly contracts: SDK.MidgardValidators;
  readonly creatorAddress: string;
  readonly creatorKeyHash: Buffer;
  readonly l1: LocalL1;
};

const loadAlwaysSucceedsContracts = (): Promise<SDK.MidgardValidators> =>
  Effect.runPromise(
    Effect.gen(function* () {
      const contracts = yield* AlwaysSucceedsContract;
      return contracts as unknown as SDK.MidgardValidators;
    }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

export const makeHarness = async (): Promise<Harness> => {
  const creator = generateEmulatorAccount({ lovelace: 60_000_000_000n });
  const emulator = new Emulator([creator], EMULATOR_PROTOCOL_PARAMETERS);
  const lucid = await makeLucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(creator.seedPhrase);
  const creatorAddress = await lucid.wallet().address();
  const credential = paymentCredentialOf(creatorAddress);
  if (credential.type !== "Key") {
    throw new Error("the emulator creator must hold a key credential");
  }
  const contracts = await loadAlwaysSucceedsContracts();
  const harness: Harness = {
    lucid,
    emulator,
    contracts,
    creatorAddress,
    creatorKeyHash: Buffer.from(credential.hash, "hex"),
    l1: await startLocalL1Observation(),
  };
  // One self-payment, so the creator always holds a second UTxO for the order to
  // reference. It is observed like every other transaction, because the reader
  // resolves *every* reference input through Kupo and a UTxO the local L1 has
  // never seen is not a reference input it can answer for.
  await submitAndObserve(
    harness,
    await lucid
      .newTx()
      .pay.ToAddress(creatorAddress, { lovelace: 5_000_000n })
      .complete({ localUPLCEval: true }),
  );
  return harness;
};

/**
 * Submits a built transaction and publishes it to the local L1 as its own block,
 * which is what gives it a chain point for the read to find.
 */
export const submitAndObserve = async (
  harness: Harness,
  tx: TxSignBuilder,
): Promise<string> => {
  const signed = await tx.sign.withWallet().complete();
  const cbor = signed.toCBOR();
  await signed.submit();
  harness.emulator.awaitBlock(1);
  harness.l1.appendBlock([cbor]);
  return signed.toHash();
};

export const publishCarriage = async (
  harness: Harness,
  plan: SDK.TxOrderCarriagePlan,
): Promise<readonly UTxO[]> => {
  const published: UTxO[] = [];
  for (const field of plan.referenced) {
    for (const publication of SDK.fieldPreimagePublicationOutputs(field.plan)) {
      const tx = await Effect.runPromise(
        SDK.buildUnsignedFieldPreimagePublicationProgram(harness.lucid, {
          publication,
          publisherAddress: harness.creatorAddress,
        }),
      );
      const txHash = await submitAndObserve(harness, tx);
      published.push(
        ...(await harness.lucid.utxosByOutRef([{ txHash, outputIndex: 0 }])),
      );
    }
    if (field.plan.tier !== "Certified") {
      continue;
    }
    const chunkUtxos = published.slice(-field.plan.publications.length);
    const tx = await Effect.runPromise(
      SDK.buildUnsignedFieldPreimageCertificationProgram(harness.lucid, {
        sourceKind: 1n,
        plan: field.plan,
        certificatePolicyId:
          harness.contracts.fieldPreimageCertificate.policyId,
        certificateAddress:
          harness.contracts.fieldPreimageCertificate.spendingScriptAddress,
        certificateWitness: {
          kind: "inline_emulator_only",
          certificateScript:
            harness.contracts.fieldPreimageCertificate.mintingScript,
        },
        chunkUtxos,
        compactCbor: "",
        witnessSetCompactCbor: "",
      }),
    );
    const txHash = await submitAndObserve(harness, tx);
    published.push(
      ...(await harness.lucid.utxosByOutRef([{ txHash, outputIndex: 0 }])),
    );
  }
  return published;
};
