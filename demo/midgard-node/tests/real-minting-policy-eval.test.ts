import { PLUTUS_V3_CANONICAL_COST_MODEL_VIEW } from "@al-ft/midgard-core";
import { readCborBytes } from "@al-ft/midgard-core/codec/cbor";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import * as SDK from "@al-ft/midgard-sdk";
import type { MidgardUplcEvalProgram } from "@al-ft/midgard-validation";
import { Constr, Data, fromHex } from "@lucid-evolution/lucid";
import { eval_program_raw } from "@lucid-evolution/uplc";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { AlwaysSucceedsContract } from "../src/services/always-succeeds.js";
import { withRealStateQueueAndOperatorContracts } from "../src/services/midgard-contracts.js";
import { TEST_AVAILABILITY_PARAMETERS } from "./helpers/availability-challenge.js";

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

/** The Flat program inside a script's CBOR byte-string wrapping. */
const flatProgram = (scriptCborHex: string): Uint8Array => {
  let bytes: Uint8Array = fromHex(scriptCborHex);
  // A Flat program starts with its version's major, a CBOR unsigned integer.
  while (bytes[0]! >> 5 === 2) {
    const { value, nextOffset } = readCborBytes(bytes, 0, "script");
    if (nextOffset !== bytes.length)
      throw new Error("script CBOR has bytes after its byte string");
    bytes = value;
  }
  return bytes;
};

/**
 * Whether the pinned Plutus V3 evaluator (aiken's, the ledger's protocol
 * version 11 semantics) runs the script applied to the script context to a
 * result. An Aiken `mint` handler that answers `False` compiles to an
 * `error`, so a refusal is a failed evaluation. The budget is far past a
 * transaction's, since only the verdict is under test.
 */
const evaluateScriptContext = (
  ourScriptCborHex: string,
  scriptContextConstr: Constr<unknown>,
): boolean => {
  const evaluate = eval_program_raw as MidgardUplcEvalProgram;
  const result = evaluate(
    flatProgram(ourScriptCborHex),
    [fromHex(Data.to(scriptContextConstr as Data))],
    3,
    11,
    BigInt64Array.from(PLUTUS_V3_CANONICAL_COST_MODEL_VIEW.map(BigInt)),
    1_000_000_000_000n,
    1_000_000_000n,
    false,
  );
  try {
    return result.ok;
  } finally {
    result.free();
  }
};

/** The validator ran to a result: the compiled `and { .. }` held. */
const expectScriptContextAccepted = (
  ourScriptCborHex: string,
  scriptContextConstr: Constr<unknown>,
) => {
  expect(evaluateScriptContext(ourScriptCborHex, scriptContextConstr)).toBe(
    true,
  );
};

/** The validator errored: the minting policy refused the context. */
const expectScriptContextRefused = (
  ourScriptCborHex: string,
  scriptContextConstr: Constr<unknown>,
) => {
  expect(evaluateScriptContext(ourScriptCborHex, scriptContextConstr)).toBe(
    false,
  );
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
          l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
          referenceScriptAuth: placeholderContracts.referenceScriptAuth,
          availabilityChallengeParameters: TEST_AVAILABILITY_PARAMETERS,
          eventHistoryProtectionDurationMs: 2_000n,
          eventHistoryBounds: {
            inlineLimitBytes: 512n,
            maxPayloadBytes: 5000n,
            maxPayloadNodes: 512n,
          },
        },
      );
    }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

describe("Plutus V3 evaluation of real Midgard minting policies", () => {
  // `hub-oracle.ak`'s `hub_mint_set_is_exact` requires the policy's token dict
  // to be EXACTLY correction-lock and hub-oracle at one shared quantity (in
  // ascending asset-name order, which is what `dict.to_pairs` yields).
  // Reading a quantity out of that two-entry dict walks it with
  // `lessThanEqualsByteString` over names of different lengths.
  it("evaluates the real hub-oracle minting policy with a ledger-shaped script context", async () => {
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

    expectScriptContextAccepted(
      contracts.hubOracle.mintingScriptCBOR,
      scriptContextConstr,
    );
  });

  describe("real fraud-proof-catalogue minting policy", () => {
    /**
     * Genesis mint: exactly one catalogue NFT coupled one-to-one with the hub
     * oracle NFT. Policy ids sort ascending here (hub < catalogue), which is
     * the order a ledger Value carries.
     */
    const genesisMint = (contracts: {
      readonly hubOracle: { readonly policyId: string };
      readonly fraudProofCatalogue: { readonly policyId: string };
    }): Map<string, Map<string, bigint>> =>
      new Map([
        [
          contracts.hubOracle.policyId,
          new Map([[SDK.HUB_ORACLE_ASSET_NAME, 1n]]),
        ],
        [
          contracts.fraudProofCatalogue.policyId,
          new Map([[SDK.FRAUD_PROOF_CATALOGUE_ASSET_NAME, 1n]]),
        ],
      ]);

    it("accepts the genesis mint coupled with a single hub-oracle NFT", async () => {
      const contracts = await loadRealContracts();

      expectScriptContextAccepted(
        contracts.fraudProofCatalogue.mintingScriptCBOR,
        mintingScriptContext(
          contracts.fraudProofCatalogue.policyId,
          genesisMint(contracts),
        ),
      );
    });

    /**
     * Each case starts from the accepted genesis mint above and breaks exactly
     * one requirement, so the refusal is attributable to that requirement and
     * an always-succeeding policy cannot satisfy the group.
     */
    const refusals: readonly {
      readonly name: string;
      readonly mutate: (
        mint: Map<string, Map<string, bigint>>,
        contracts: {
          readonly hubOracle: { readonly policyId: string };
          readonly fraudProofCatalogue: { readonly policyId: string };
        },
      ) => void;
    }[] = [
      {
        name: "a standalone post-genesis re-mint with no hub-oracle NFT",
        mutate: (mint, contracts) => {
          mint.delete(contracts.hubOracle.policyId);
        },
      },
      {
        name: "a hub-oracle quantity other than one",
        mutate: (mint, contracts) => {
          mint.set(
            contracts.hubOracle.policyId,
            new Map([[SDK.HUB_ORACLE_ASSET_NAME, 2n]]),
          );
        },
      },
      {
        name: "a duplicated catalogue token",
        mutate: (mint, contracts) => {
          mint.set(
            contracts.fraudProofCatalogue.policyId,
            new Map([[SDK.FRAUD_PROOF_CATALOGUE_ASSET_NAME, 2n]]),
          );
        },
      },
      {
        name: "a catalogue asset name that is not the catalogue NFT",
        mutate: (mint, contracts) => {
          mint.set(
            contracts.fraudProofCatalogue.policyId,
            new Map([
              [`${SDK.FRAUD_PROOF_CATALOGUE_ASSET_NAME.slice(0, -2)}ff`, 1n],
            ]),
          );
        },
      },
    ];

    it.each(refusals)("refuses $name", async ({ mutate }) => {
      const contracts = await loadRealContracts();
      const mint = genesisMint(contracts);
      mutate(mint, contracts);

      expectScriptContextRefused(
        contracts.fraudProofCatalogue.mintingScriptCBOR,
        mintingScriptContext(contracts.fraudProofCatalogue.policyId, mint),
      );
    });
  });
});
