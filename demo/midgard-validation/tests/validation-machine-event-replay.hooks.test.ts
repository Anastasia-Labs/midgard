/**
 * The verdict replay's script-run hooks: a replay takes one at most, and
 * only in verdict mode.
 */
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { Effect, Either } from "effect";
import { describe, expect, it } from "vitest";

import { replayValidationVerdictEvent } from "../src/index.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  makeNativeTx,
  makeOutput,
  outRefFromByte,
} from "./validation-fixtures.js";

const spent = outRefFromByte(0x31);
const output = makeOutput(FUNDED_OUTPUT_LOVELACE);
const transaction = makeNativeTx({ spendInputs: [spent], outputs: [output] });

type Input = Parameters<typeof replayValidationVerdictEvent>[0];

const replay = (hooks: Partial<Input>) =>
  Effect.runPromise(
    Effect.either(
      replayValidationVerdictEvent({
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        canonicalTransactionCbor: transaction.txCbor,
        ledgerWitnessEntries: [{ outRef: spent, output }],
        blockTimeMs: 1_750_000_000_000,
        expectedNetworkId: 0n,
        minFeeA: 0n,
        minFeeB: 0n,
        blockSlot: 100n,
        l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
        sourceKind: "normal",
        ...hooks,
      }),
    ),
  );

const committedScriptRun = () => undefined;
const evaluateScriptRun = () =>
  Effect.succeed({
    kind: "accepted" as const,
    budget: { cpu: 0n, memory: 0n },
  });

const refusal = (result: Awaited<ReturnType<typeof replay>>) =>
  Either.isLeft(result) ? result.left.message : undefined;

describe("verdict replay script-run hooks", () => {
  it("replays with either hook alone in verdict mode", async () => {
    for (const hooks of [{ committedScriptRun }, { evaluateScriptRun }]) {
      const result = await replay(hooks);
      expect(Either.isRight(result) && result.right.verdict).toBe("accepted");
    }
  });

  it("refuses both hooks at once", async () => {
    expect(
      refusal(await replay({ committedScriptRun, evaluateScriptRun })),
    ).toBe("a replay takes one script-run hook at most");
  });

  it("refuses either hook outside verdict mode", async () => {
    for (const hooks of [{ committedScriptRun }, { evaluateScriptRun }])
      expect(
        refusal(await replay({ ...hooks, cekExecutionMode: "proof" })),
      ).toBe("a script-run hook runs in verdict mode only");
  });
});
