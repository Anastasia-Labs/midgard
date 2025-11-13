import { expect, test, describe, beforeAll } from "vitest";
import { Effect } from "effect";
import { Data } from "@lucid-evolution/lucid";
import {
  initializationTestCase,
  InitializationResult,
} from "./initTestCase.js";
import { LucidContext, makeLucidContext } from "./service/lucidContext.js";
import { NodeDatum } from "../src/linked-list.js";

describe("Midgard Initialization", () => {
  let context: LucidContext;
  let result: InitializationResult;

  // Run initialization once before all tests
  beforeAll(async () => {
    const program = Effect.gen(function* () {
      const ctx = yield* makeLucidContext();
      const res = yield* initializationTestCase(ctx);
      return { context: ctx, result: res };
    });

    const data = await Effect.runPromise(program);
    context = data.context;
    result = data.result;
  }, 30000); // 30 second timeout for initialization

  test("should complete initialization transaction successfully", () => {
    expect(result.txHash).toBeDefined();
    expect(typeof result.txHash).toBe("string");
    expect(result.txHash.length).toBe(64); // Valid tx hash
  });

  test("should consume nonce UTxO", () => {
    expect(result.nonceUtxoConsumed).toBe(true);
  });

  // test("should mint all 8 contract NFTs", async () => {
  //   const { lucid } = context;
  //   const { validators } = result;

  //   // Check each contract has exactly 1 NFT
  //   const contracts = [
  //     { name: "Hub Oracle", address: validators.hubOracle.spendAddress },
  //     { name: "State Queue", address: validators.stateQueue.spendAddress },
  //     { name: "Settlement Queue", address: validators.settlementQueue.spendAddress },
  //     { name: "Registered Operators", address: validators.registeredOperators.mintAddress },
  //     { name: "Active Operators", address: validators.activeOperators.spendAddress },
  //     { name: "Retired Operators", address: validators.retiredOperators.mintAddress },
  //     { name: "Scheduler", address: validators.scheduler.spendAddress },
  //     // { name: "Escape Hatch", address: validators.escapeHatch.spendAddress },
  //     // { name: "Fraud Proof Catalogue", address: validators.fraudProofCatalogue.spendAddress },
  //   ];

  //   for (const contract of contracts) {
  //     const utxos = await lucid.utxosAt(contract.address);
  //     expect(utxos, `${contract.name} should have exactly 1 NFT`).toHaveLength(1);
  //   }
  // });

  test("should create root nodes with correct datum structure", async () => {
    const { lucid } = context;
    const { validators } = result;

    // Check state queue root node
    const stateQueueUtxos = await lucid.utxosAt(
      validators.stateQueue.spendAddress,
    );
    expect(stateQueueUtxos).toHaveLength(1);

    const stateQueueDatum = Data.from(stateQueueUtxos[0].datum!, NodeDatum);
    expect(stateQueueDatum.key).toBe("Empty");
    expect(stateQueueDatum.next).toBe("Empty");
    expect(stateQueueDatum.data).toBeDefined();
  });

  // test("should create hub oracle with all policy IDs", async () => {
  //   const { lucid } = context;
  //   const { validators } = result;

  //   const hubOracleUtxos = await lucid.utxosAt(validators.hubOracle.mintAddress);
  //   expect(hubOracleUtxos).toHaveLength(1);

  //   const datum = Data.from(hubOracleUtxos[0].datum!, HubOracleDatum);

  //   // Verify all policy IDs
  //   expect(datum.registeredOperators).toBe(result.registeredOperatorsPolicyId);
  //   expect(datum.activeOperators).toBe(result.activeOperatorsPolicyId);
  //   expect(datum.retiredOperators).toBe(result.retiredOperatorsPolicyId);
  //   expect(datum.scheduler).toBe(result.schedulerPolicyId);
  //   expect(datum.stateQueue).toBe(result.stateQueuePolicyId);
  //   expect(datum.fraudProofCatalogue).toBe(result.fraudProofCataloguePolicyId);
  // });

  test("should initialize empty operator lists (only root nodes)", async () => {
    const { lucid } = context;
    const { validators } = result;

    // All operator lists should have exactly 1 UTxO (the root)
    const registeredUtxos = await lucid.utxosAt(
      validators.registeredOperators.mintAddress,
    );
    const activeUtxos = await lucid.utxosAt(
      validators.activeOperators.spendAddress,
    );
    const retiredUtxos = await lucid.utxosAt(
      validators.retiredOperators.mintAddress,
    );

    expect(
      registeredUtxos,
      "Registered operators should have only root",
    ).toHaveLength(1);
    expect(activeUtxos, "Active operators should have only root").toHaveLength(
      1,
    );
    expect(
      retiredUtxos,
      "Retired operators should have only root",
    ).toHaveLength(1);
  });

  test("should initialize empty settlement queue", async () => {
    const { lucid } = context;
    const { validators } = result;

    const settlementQueueUtxos = await lucid.utxosAt(
      validators.settlementQueue.spendAddress,
    );
    expect(
      settlementQueueUtxos,
      "Settlement queue should have only root",
    ).toHaveLength(1);
  });
});
