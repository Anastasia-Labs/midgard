/**
 * The node-runtime reference-script publication (startup and reconcile) and
 * the per-command deploy read the deployment's initialization nonce from its
 * contracts, and never fund the reference-script wallet's top-up with it: an
 * operator wallet whose only UTxO is that nonce is refused, and the nonce
 * stays unspent.
 */
import * as SDK from "@al-ft/midgard-sdk";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";
import { expect, it } from "vitest";

import {
  deployReferenceScriptCommandProgram,
  ensureNodeRuntimeReferenceScriptsProgram,
} from "../src/transactions/reference-scripts.js";
import { runWithoutFollower } from "./helpers/intent-journal.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";

const outRef = (utxo: UTxO): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

type EntryPoint = (
  reference: LucidEvolution,
  contracts: SDK.MidgardValidators,
  authPolicy: SDK.ReferenceScriptAuthPolicy,
  operator: LucidEvolution,
) => ReturnType<typeof ensureNodeRuntimeReferenceScriptsProgram>;

const entryPoints: readonly (readonly [string, EntryPoint])[] = [
  [
    "node-runtime publication",
    (reference, contracts, authPolicy, operator) =>
      ensureNodeRuntimeReferenceScriptsProgram(
        reference,
        contracts,
        authPolicy,
        operator,
        undefined,
        1,
      ),
  ],
  [
    "protocol-init deploy command",
    (reference, contracts, authPolicy, operator) =>
      deployReferenceScriptCommandProgram(
        reference,
        contracts,
        "protocol-init",
        authPolicy,
        operator,
        undefined,
        1,
      ),
  ],
];

it.each(entryPoints)(
  "the %s never funds the top-up with the deployment's initialization nonce",
  async (_name, entryPoint) => {
    const operator = generateEmulatorAccount({ lovelace: 500_000_000n });
    const reference = generateEmulatorAccount({ lovelace: 0n });
    const emulator = new Emulator([operator, reference]);
    const operatorLucid = await Lucid(emulator, "Custom");
    operatorLucid.selectWallet.fromSeed(operator.seedPhrase);
    const referenceLucid = await Lucid(emulator, "Custom");
    referenceLucid.selectWallet.fromSeed(reference.seedPhrase);
    const authPolicy = await SDK.createReferenceScriptAuthPolicy(
      referenceLucid,
      emulator.now(),
      24 * 60 * 60 * 1_000,
    );
    // The operator wallet's one UTxO is the deployment's nonce.
    const operatorBefore = await operatorLucid.utxosAt(operator.address);
    expect(operatorBefore).toHaveLength(1);
    const nonce = operatorBefore[0]!;
    const contracts = await loadRealMidgardContractsForTest(nonce);

    const result = await runWithoutFollower(
      Effect.either(
        entryPoint(referenceLucid, contracts, authPolicy, operatorLucid),
      ),
    );

    expect(Either.isLeft(result)).toBe(true);
    const failure = Either.isLeft(result) ? result.left : undefined;
    expect(failure).toBeInstanceOf(SDK.StateQueueError);
    expect((failure as SDK.StateQueueError).message).toContain(
      "No operator wallet funding UTxOs available",
    );
    expect((await operatorLucid.utxosAt(operator.address)).map(outRef)).toEqual(
      [outRef(nonce)],
    );
    expect(
      (await referenceLucid.utxosAt(reference.address)).filter(
        (utxo) => (utxo.assets.lovelace ?? 0n) > 0n,
      ),
    ).toEqual([]);
  },
  120_000,
);
