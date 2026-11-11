/**
 * Every node-runtime reference script fits a real L1 publication transaction.
 *
 * This is GOAL_SPEC §8.1 row CG1: "every parameterized hub/control validator
 * fits a real 16,384-byte publication transaction".
 *
 *   - the roster is re-derived from `nodeRuntimeReferenceScriptTargets`, never
 *     hardcoded, so a validator added to or removed from the node's runtime
 *     reference-script set changes this check automatically;
 *   - the contracts are a genuinely real `MidgardValidators` bundle built from
 *     the working-tree blueprint (`loadRealMidgardContractsForTest`), not the
 *     AlwaysSucceeds stand-in, so every applied script is the real one;
 *   - each target is published through
 *     `completeReferenceScriptPublicationTxProgram` — the same builder the
 *     node's production reference-script publication path calls — signed, and
 *     the COMPLETE SIGNED transaction is measured. Raw script size is not the
 *     bound; the signed publication envelope is;
 *   - the emulator's `maxTxSize` is pinned to the real L1 16,384-byte limit
 *     rather than the emulator's relaxed default, so a transaction that would
 *     be rejected on L1 is rejected here too;
 *   - targets are processed sequentially so each publication's funding
 *     selection sees the previous one's confirmed wallet UTxOs.
 *
 * Blueprint independence: nothing here pins a hash, a byte count or a roster
 * name. It re-derives all of them, so it re-runs against any regenerated
 * blueprint.
 *
 * The one-shot outref is fixed to the all-zero reference rather than a live
 * wallet UTxO. The one-shot parameter feeds the applied scripts, so a wallet-
 * derived outref would make every measured transaction depend on the emulator
 * seed.
 */
import { existsSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import * as SDK from "@al-ft/midgard-sdk";
import {
  completeReferenceScriptPublicationTxProgram,
  createReferenceScriptAuthPolicy,
  referenceScriptPublicationFundingTarget,
} from "@al-ft/midgard-sdk";
import { selectPlainFundingUtxos } from "@al-ft/midgard-test-support/reference-script-funding";
import { Emulator, generateEmulatorAccount } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

// `ReferenceScriptTarget` is the SDK's type, re-exported by the node's own
// reference-scripts module; `@lucid-evolution/lucid` does not export it.
import {
  nodeRuntimeReferenceScriptTargets,
  type ReferenceScriptTarget,
} from "../src/transactions/reference-scripts.js";
import {
  createMainnetEmulatorLucid,
  MAINNET_PROTOCOL_PARAMETERS,
} from "./helpers/mainnet-protocol-parameters.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";

const moduleDirectory = dirname(fileURLToPath(import.meta.url));
const repositoryRoot = resolve(moduleDirectory, "../../..");

/**
 * `onchain/aiken/plutus.json` is gitignored. A checkout without it cannot
 * build a real contract bundle at all, and this check skips there. A skipped
 * measurement is honest — a measurement against the AlwaysSucceeds stand-in
 * would not be.
 */
const blueprintPath =
  process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
  resolve(repositoryRoot, "onchain/aiken/plutus.json");
const blueprintPresent = existsSync(blueprintPath);

/** The bound is the real L1 limit, not the emulator's relaxed default. */
const MAX_L1_TX_BYTES = 16_384;

const EMULATOR_PROTOCOL_PARAMETERS = {
  ...MAINNET_PROTOCOL_PARAMETERS,
  maxTxSize: MAX_L1_TX_BYTES,
  maxTxExMem: 16_500_000n,
  maxTxExSteps: 10_000_000_000n,
  maxCollateralInputs: 3,
} as const;

/** Fixed, not wallet-derived — see the header note. */
const ONE_SHOT_OUT_REF = {
  txHash: "0".repeat(64),
  outputIndex: 0,
} as const;

describe.skipIf(!blueprintPresent)(
  "every node-runtime control validator fits a real 16,384-byte publication transaction",
  () => {
    it("publishes every roster target under the real L1 envelope", async () => {
      const operator = generateEmulatorAccount({ lovelace: 30_000_000_000n });
      const referenceScripts = generateEmulatorAccount({
        lovelace: 50_000_000_000n,
      });
      const emulator = new Emulator(
        [operator, referenceScripts],
        EMULATOR_PROTOCOL_PARAMETERS,
      );
      const lucid = await createMainnetEmulatorLucid(emulator, "Preprod");
      lucid.selectWallet.fromSeed(referenceScripts.seedPhrase);

      // A fresh deployer-chosen native timelock policy, exactly as a real
      // deployment would create. It is a Plutus parameter for da-attestation
      // (both legs), so it shapes that validator's applied script.
      const authPolicy = await createReferenceScriptAuthPolicy(
        lucid,
        emulator.now(),
      );

      // The whole policy object, not one of its fields.
      // `ReferenceScriptAuthPolicy` is structurally a superset of
      // `SDK.MintingValidator` ({ mintingScriptCBOR, mintingScript, policyId }),
      // so it is passed as-is; da-attestation's applied hash embeds its
      // `policyId`.
      //
      // This argument MUST be passed. `loadRealMidgardContractsForTest`'s
      // second parameter is optional and silently falls back to the
      // AlwaysSucceeds placeholder's auth script when omitted, which binds
      // da-attestation's applied script to the placeholder policy instead of
      // the deployer-chosen one — a wrong script measured by a run that still
      // reports green. Passing a `Script` instead of the policy
      // object fails loudly ("Could not serialize the data: Unsupported
      // type"); passing nothing fails silently. Only the latter is dangerous.
      const contracts = await loadRealMidgardContractsForTest(
        ONE_SHOT_OUT_REF,
        authPolicy as SDK.MintingValidator,
      );
      const targets = nodeRuntimeReferenceScriptTargets(contracts);
      expect(targets.length).toBeGreaterThan(0);
      expect(new Set(targets.map(({ name }) => name)).size).toEqual(
        targets.length,
      );

      const referenceScriptsAddress = await lucid.wallet().address();

      // Sequential on purpose: each publication's funding selection must see
      // the previous one's confirmed change, the way a real deployment run
      // publishes one target at a time.
      for (const target of targets as readonly ReferenceScriptTarget[]) {
        const selectedFundingInputs = selectPlainFundingUtxos(
          await lucid.wallet().getUtxos(),
          referenceScriptPublicationFundingTarget(1),
        );
        expect(
          selectedFundingInputs.length,
          `no plain-Ada funding input available for ${target.name}`,
        ).toBeGreaterThan(0);

        const { tx, layout } = await Effect.runPromise(
          completeReferenceScriptPublicationTxProgram({
            lucid,
            selectedFundingInputs,
            walletAddress: referenceScriptsAddress,
            referenceScriptsAddress,
            missingTargets: [target],
            authPolicy,
          }),
        );
        const localOutput = layout.localReferenceOutputs.get(target.name);
        expect(
          localOutput,
          `publication transaction omitted the ${target.name} reference-script output`,
        ).toBeDefined();

        const signed = await tx.sign.withWallet().complete();
        // CBOR hex is two characters per byte; the transaction as submitted is
        // the thing the ledger's size rule applies to.
        const completeSignedBytes = signed.toCBOR().length / 2;
        const l1ByteMarginBytes = MAX_L1_TX_BYTES - completeSignedBytes;

        // The acceptance clause, asserted per target rather than in aggregate:
        // a single oversize control validator fails the check outright.
        expect(
          l1ByteMarginBytes,
          `${target.name} is ${completeSignedBytes.toString()} signed bytes and does not fit the ${MAX_L1_TX_BYTES.toString()}-byte L1 envelope`,
        ).toBeGreaterThan(0);

        // Submitting is what makes this a real fit rather than an arithmetic
        // one: the emulator applies its pinned maxTxSize on submit.
        const txHash = await signed.submit();
        await emulator.awaitTx(txHash);
        const published = await lucid.utxosByOutRef([
          { txHash, outputIndex: localOutput!.outputIndex },
        ]);
        expect(
          published.length,
          `expected exactly one live ${target.name} reference-script UTxO`,
        ).toEqual(1);
      }
    });
  },
);
