import { computeMidgardNativeTxId } from "@al-ft/midgard-core";
import { encodeMidgardForcedTxCanonical } from "@al-ft/midgard-core/codec/forced";
import { describe, expect, it } from "vitest";

import { admitNativeScriptInvalidForcedArtifact } from "../src/native-script-invalid/forced-artifact.js";
import { run } from "./native-script-invalid-wrongful-rejection-lifecycle.run.js";
import {
  buildNativeScriptInvalidTransaction,
  setup,
} from "./native-script-invalid-wrongful-rejection-lifecycle.setup.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { nodeForcedVerdict } from "./support/emulator/node-forced-verdict.js";

/**
 * A forced WitnessNativeScriptFalse reason names a field-6 script, and
 * nativeScriptInvalid reopens exactly that script and evaluates it against
 * the transaction's signers. The fixture puts a true `all []` script before a
 * false one, and the transaction spends an input so the classifier reaches
 * the witness scripts. The verdict is the one the node's classifier writes,
 * so the suite fails if the writer names any script but the false one: the
 * true script one position early convicts; the written script is refused on
 * chain.
 */

const shape = { falseScript: true, prefixCount: 1, spendInput: true } as const;

const writtenScriptIndex = async (): Promise<bigint> => {
  const { tx } = buildNativeScriptInvalidTransaction(shape);
  const verdict = await nodeForcedVerdict({
    transactionId: computeMidgardNativeTxId(tx),
    forcedCanonicalCbor: encodeMidgardForcedTxCanonical(tx),
  });
  expect(verdict).toStrictEqual({
    ForcedTxInvalid: {
      reason: { WitnessNativeScriptFalse: { script_index: 1n } },
    },
  });
  return 1n;
};

describe("forced WitnessNativeScriptFalse coordinate the node writes", () => {
  it("convicts a coordinate one script early, where the script is true", async () => {
    const f = await setup({
      ...shape,
      committedScriptIndex: (await writtenScriptIndex()) - 1n,
    });
    // The prover's own admission finds the contradiction at that script.
    await admitNativeScriptInvalidForcedArtifact(
      JSON.parse(JSON.stringify(f.artifact)),
    );
    // Init, bind, grammar, signer evaluation, proof mint and block removal.
    await run(f);
  }, 600_000);

  it("refuses the written coordinate on chain", async () => {
    const f = await setup({
      ...shape,
      committedScriptIndex: await writtenScriptIndex(),
    });
    // The written script is false: the rejection holds.
    await expect(
      admitNativeScriptInvalidForcedArtifact(f.artifact),
    ).rejects.toThrow("no contradiction");
    // Init, bind and the grammar pass; step 03 evaluates the script against
    // the signers and returns the terminal rule, which convicts a forced
    // rejection only when the script is satisfied, so it returns false.
    await expectOnchainRefusal(
      () => run(f, true),
      /^Validator returned false$/u,
    );
  }, 600_000);
});
