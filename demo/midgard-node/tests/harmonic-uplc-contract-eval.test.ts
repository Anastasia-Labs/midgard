import { describe, expect, it } from "vitest";
import { Effect } from "effect";
import { Application, parseUPLC, UPLCConst } from "@harmoniclabs/uplc";
import { CEKConst, Machine } from "@harmoniclabs/plutus-machine";
import { dataFromCbor } from "@harmoniclabs/plutus-data";
import { Constr, Data, fromHex } from "@lucid-evolution/lucid";
import * as SDK from "@al-ft/midgard-sdk";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import { withRealStateQueueAndOperatorContracts } from "@/services/midgard-contracts.js";

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
      );
    }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

describe("@harmoniclabs/uplc evaluation against real Midgard contracts", () => {
  it("evaluates the real hub-oracle minting policy with a ledger-shaped script context", async () => {
    const contracts = await loadRealContracts();
    const hubOracleMint = new Map([
      [
        contracts.hubOracle.policyId,
        new Map([[SDK.HUB_ORACLE_ASSET_NAME, 1n]]),
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
