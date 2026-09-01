import * as SDK from "@al-ft/midgard-sdk";
import { dataFromCbor } from "@harmoniclabs/plutus-data";
import { CEKConst, Machine } from "@harmoniclabs/plutus-machine";
import { Application, parseUPLC, UPLCConst } from "@harmoniclabs/uplc";
import { Constr, Data, fromHex } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import { withRealStateQueueAndOperatorContracts } from "@/services/midgard-contracts.js";

import { TEST_AVAILABILITY_PARAMETERS_V1 } from "./helpers/availability-challenge-v1.js";

const oneShotOutRef = {
  txHash: "aa".repeat(32),
  outputIndex: 0,
} as const;

const none = new Constr(1, []);
const bool = (value: boolean) => new Constr(value ? 1 : 0, []);
const validityRange = new Constr(0, [
  new Constr(0, [new Constr(1, [0n]), bool(true)]),
  new Constr(0, [new Constr(1, [999_999n]), bool(false)]),
]);

const emptyTransaction = (
  mint: Map<string, Map<string, bigint>>,
  inputs: unknown[] = [],
) =>
  new Constr(0, [
    inputs,
    [],
    [],
    0n,
    mint,
    [],
    new Map(),
    validityRange,
    [],
    new Map(),
    new Map(),
    "00".repeat(32),
    new Map(),
    [],
    none,
    none,
  ]);

const mintingScriptContext = (
  policyId: string,
  mint: Map<string, Map<string, bigint>>,
  inputs: unknown[] = [],
) =>
  new Constr(0, [
    emptyTransaction(mint, inputs),
    new Constr(0, []),
    new Constr(0, [policyId]),
  ]);

const dataConst = (value: any) =>
  UPLCConst.data(dataFromCbor(fromHex(Data.to(value)) as Uint8Array));

const expectScriptContextEvaluatesToCekConst = (
  ourScriptCborHex: string,
  scriptContextConstr: Constr<unknown>,
) => {
  const uplc = parseUPLC(fromHex(ourScriptCborHex), "cbor").body;
  const dataEncodedScriptContext = dataConst(scriptContextConstr);
  const applied_1 = new Application(uplc, dataEncodedScriptContext);
  const result_1 = Machine.eval(applied_1);

  expect(result_1.result instanceof CEKConst).toBe(true);
};

const loadRealContracts = () =>
  Effect.runPromise(
    Effect.gen(function* () {
      const placeholderContracts = yield* AlwaysSucceedsContract;
      return yield* withRealStateQueueAndOperatorContracts(
        "Preprod",
        placeholderContracts,
        oneShotOutRef,
        {
          referenceScriptAuth: placeholderContracts.referenceScriptAuth,
          availabilityChallengeParameters: TEST_AVAILABILITY_PARAMETERS_V1,
        },
      );
    }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

describe("@harmoniclabs/uplc evaluation against real Midgard contracts", () => {
  // SKIPPED because @harmoniclabs/plutus-machine cannot evaluate this policy
  // correctly — the Midgard validator is fine, its evaluator is not.
  //
  // `hub-oracle.ak`'s `hub_mint_set_is_exact` now requires the policy's token
  // dict to be EXACTLY correction-lock and hub-oracle at one shared quantity
  // (the mint set below, in ascending asset-name order, which is what
  // `dict.to_pairs` yields and what `incompleteHubOracleInitTxProgram`
  // actually mints). Reading a quantity out of that two-entry dict makes the
  // compiled `quantity_of` walk it with `lessThanEqualsByteString`, and
  // `@harmoniclabs/plutus-machine`'s implementation of that builtin is wrong:
  //
  //   BnCEK.prototype.lessThanByteString:
  //     if (aBytes.length < bBytes.length) return CEKConst.bool(true);
  //
  // It orders by LENGTH first, where Plutus orders lexicographically. So the
  // machine answers `true` to both "MIDGARD_HUB_ORACLE" (18 bytes) <=
  // "MIDGARD_CORRECTION_LOCK" (23 bytes) AND its converse — not a total order
  // at all. The dict walk therefore stops at the first key, reports quantity 0 for
  // the hub-oracle NFT, and `hub_mint_set_is_exact` compares the real dict
  // against a zero-quantity expectation and fails. The single-token mint this
  // test used before the exact-set rule never compared two different-length
  // names, which is why the bug only surfaces now. The defect is present in
  // every published version through 3.0.3, so bumping the pin does not fix it.
  //
  // The claim this test was making is not lost: the same policy is evaluated
  // against a real ledger script context, by the real evaluator, in
  // `tests/initialization-emulator.test.ts` > "builds the hub-oracle mint
  // fragment in isolation" (`complete({ localUPLCEval: true })`), which passes
  // with exactly this two-token mint. Re-enable this case (drop the `.skip`,
  // change nothing else) once the upstream builtin is fixed.
  it.skip("evaluates the real hub-oracle minting policy with a ledger-shaped script context", async () => {
    const contracts = await loadRealContracts();
    const hubOracleMint = new Map([
      [
        contracts.hubOracle.policyId,
        new Map([
          [SDK.CORRECTION_LOCK_ASSET_NAME, 1n],
          [SDK.HUB_ORACLE_ASSET_NAME, 1n],
        ]),
      ],
    ]);
    const outputReference = new Constr(0, [
      oneShotOutRef.txHash,
      BigInt(oneShotOutRef.outputIndex),
    ]);
    const dummyAddress = new Constr(0, [
      new Constr(1, ["44".repeat(28)]),
      none,
    ]);
    const dummyOutput = new Constr(0, [
      dummyAddress,
      new Map(),
      new Constr(0, []),
      none,
    ]);
    const input = new Constr(0, [outputReference, dummyOutput]);
    const scriptContextConstr = mintingScriptContext(
      contracts.hubOracle.policyId,
      hubOracleMint,
      [input],
    );

    expectScriptContextEvaluatesToCekConst(
      contracts.hubOracle.mintingScriptCBOR,
      scriptContextConstr,
    );
  });

  it("evaluates the real fraud-proof-catalogue minting policy with a ledger-shaped script context", async () => {
    const contracts = await loadRealContracts();
    const mint = new Map([
      [
        contracts.hubOracle.policyId,
        new Map([[SDK.HUB_ORACLE_ASSET_NAME, 1n]]),
      ],
      [
        contracts.fraudProofCatalogue.policyId,
        new Map([[SDK.FRAUD_PROOF_CATALOGUE_ASSET_NAME, 1n]]),
      ],
    ]);
    const scriptContextConstr = mintingScriptContext(
      contracts.fraudProofCatalogue.policyId,
      mint,
    );

    expectScriptContextEvaluatesToCekConst(
      contracts.fraudProofCatalogue.mintingScriptCBOR,
      scriptContextConstr,
    );
  });
});
