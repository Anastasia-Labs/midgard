import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  applyDoubleCborEncoding,
  Data,
  Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  type LucidEvolution,
  mintingPolicyToId,
  type Network,
  PROTOCOL_PARAMETERS_DEFAULT,
  type Script,
  toUnit,
  type TxSignBuilder,
  type UTxO,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  type AuthenticatedValidator,
  type BuildSchedulerRefreshTxConfig,
  encodeSchedulerDatumForChain,
  SCHEDULER_ASSET_NAME,
} from "../src/index.js";

const moduleDir = dirname(fileURLToPath(import.meta.url));

const repoRoot = resolve(moduleDir, "../../..");

const alwaysSucceedsBlueprintPath = resolve(
  repoRoot,
  "demo/midgard-node/blueprints/always-succeeds/plutus.json",
);

const NETWORK: Network = "Custom";

const EMULATOR_PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  maxCollateralInputs: 3,
} as const;

/**
 * The scheduler validator is scaffolded with the shared always-succeeds script
 * so that a genuine Plutus V3 spend — script witness, redeemer, collateral,
 * local UPLC evaluation — runs through Lucid. The refresh builder's contract is
 * the transaction layout, not the validator's own logic.
 */
const loadAlwaysSucceedsScript = (): Script => {
  const blueprint = JSON.parse(
    readFileSync(alwaysSucceedsBlueprintPath, "utf8"),
  ) as { readonly validators: readonly { readonly compiledCode: string }[] };
  const compiledCode = blueprint.validators[0]?.compiledCode;
  if (compiledCode === undefined) {
    throw new Error("always-succeeds blueprint carries no validators");
  }
  return { type: "PlutusV3", script: applyDoubleCborEncoding(compiledCode) };
};

export const alwaysSucceedsSchedulerValidator = (): AuthenticatedValidator => {
  const script = loadAlwaysSucceedsScript();
  return {
    policyId: mintingPolicyToId(script),
    spendingScriptAddress: validatorToAddress(NETWORK, script),
    spendingScriptHash: validatorToScriptHash(script),
    spendingScriptCBOR: script.script,
    mintingScriptCBOR: script.script,
    spendingScript: script,
    mintingScript: script,
  } as AuthenticatedValidator;
};

type OutRef = { readonly txHash: string; readonly outputIndex: number };

export const outRefKey = (outRef: OutRef): string =>
  `${outRef.txHash}#${outRef.outputIndex.toString()}`;

type CmlInputs = {
  readonly len: () => number;
  readonly get: (index: number) => {
    readonly transaction_id: () => { readonly to_hex: () => string };
    readonly index: () => bigint | number;
  };
};

const cmlOutRefKeys = (inputs: CmlInputs | undefined): readonly string[] => {
  if (inputs === undefined) {
    return [];
  }
  const keys: string[] = [];
  for (let index = 0; index < inputs.len(); index += 1) {
    const input = inputs.get(index);
    keys.push(`${input.transaction_id().to_hex()}#${input.index().toString()}`);
  }
  return keys;
};

const bodyOf = (tx: TxSignBuilder) =>
  tx.toTransaction().body() as unknown as {
    readonly inputs: () => CmlInputs;
    readonly reference_inputs: () => CmlInputs | undefined;
  };

/** Indices of the transaction body Lucid actually serialized. */
export const assembledIndices = (tx: TxSignBuilder) => {
  const body = bodyOf(tx);
  return {
    inputs: cmlOutRefKeys(body.inputs()),
    referenceInputs: cmlOutRefKeys(body.reference_inputs()),
  };
};

export const requireIndex = (
  keys: readonly string[],
  outRef: OutRef,
): bigint => {
  const index = keys.indexOf(outRefKey(outRef));
  if (index < 0) {
    throw new Error(
      `${outRefKey(outRef)} is absent from the assembled transaction: ${keys.join(", ")}`,
    );
  }
  return BigInt(index);
};

/**
 * Independent reference model for the ledger's canonical input ordering:
 * ascending by transaction id, then by output index. Asserting the assembled
 * body against this as well as asserting the builder against the assembled body
 * keeps the pair of claims from collapsing into one.
 */
export const canonicalOutRefOrder = (
  keys: readonly string[],
): readonly string[] =>
  [...keys].sort((left, right) => {
    const [leftHash = "", leftIndex = "0"] = left.split("#");
    const [rightHash = "", rightIndex = "0"] = right.split("#");
    if (leftHash !== rightHash) {
      return leftHash < rightHash ? -1 : 1;
    }
    return Number(leftIndex) - Number(rightIndex);
  });

type SchedulerScene = {
  readonly lucid: LucidEvolution;
  readonly emulator: Emulator;
  readonly scheduler: AuthenticatedValidator;
  readonly schedulerUnit: string;
  readonly schedulerInput: UTxO;
  readonly activeTail: UTxO;
  readonly activeRoot: UTxO;
  readonly registeredWitness: UTxO;
  readonly schedulerScriptRef: UTxO;
  readonly operatorKeyHash: string;
  readonly baseConfig: Omit<BuildSchedulerRefreshTxConfig, "selection">;
};

/**
 * Publishes, on the emulator, the scheduler UTxO (carrying the scheduler NFT),
 * the three witness UTxOs the three selections read, and a scheduler reference
 * script. All of them live at the scheduler script address so that Lucid's coin
 * selection cannot pull a witness UTxO into the input set.
 */
export const setupSchedulerScene = async (): Promise<SchedulerScene> => {
  const account = generateEmulatorAccount({ lovelace: 40_000_000_000n });
  const emulator = new Emulator([account], EMULATOR_PROTOCOL_PARAMETERS);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const paymentCredential = getAddressDetails(
    account.address,
  ).paymentCredential;
  if (paymentCredential === undefined || paymentCredential.type !== "Key") {
    throw new Error("expected the emulator wallet to expose a payment key");
  }
  const scheduler = alwaysSucceedsSchedulerValidator();
  const schedulerUnit = toUnit(scheduler.policyId, SCHEDULER_ASSET_NAME);
  const address = scheduler.spendingScriptAddress;
  const tagCbor = (label: string): string =>
    Data.to(Buffer.from(label, "utf8").toString("hex"));
  const tag = (label: string) => ({
    kind: "inline" as const,
    value: tagCbor(label),
  });
  const setup = await (
    await lucid
      .newTx()
      .mintAssets({ [schedulerUnit]: 1n }, Data.void())
      .attach.MintingPolicy(scheduler.mintingScript)
      .pay.ToContract(address, tag("scheduler"), {
        lovelace: 20_000_000n,
        [schedulerUnit]: 1n,
      })
      .pay.ToContract(address, tag("active-tail"), { lovelace: 5_000_000n })
      .pay.ToContract(address, tag("active-root"), { lovelace: 6_000_000n })
      .pay.ToContract(address, tag("registered-witness"), {
        lovelace: 7_000_000n,
      })
      .pay.ToContract(
        address,
        tag("scheduler-script-ref"),
        { lovelace: 40_000_000n },
        scheduler.spendingScript,
      )
      .complete()
  ).sign
    .withWallet()
    .complete();
  await lucid.awaitTx(await setup.submit());
  emulator.awaitBlock(1);

  const published = await lucid.utxosAt(address);
  const byTag = (label: string): UTxO => {
    const wanted = tagCbor(label);
    const found = published.filter((utxo) => utxo.datum === wanted);
    if (found.length !== 1) {
      throw new Error(
        `expected exactly one published ${label} UTxO, got ${found.length.toString()}`,
      );
    }
    return found[0]!;
  };
  const schedulerInput = byTag("scheduler");
  expect(schedulerInput.assets[schedulerUnit]).toBe(1n);
  const schedulerScriptRef = byTag("scheduler-script-ref");
  expect(schedulerScriptRef.scriptRef?.script).toBe(
    scheduler.spendingScript.script,
  );

  const validFrom = BigInt(emulator.now());
  return {
    lucid,
    emulator,
    scheduler,
    schedulerUnit,
    schedulerInput,
    activeTail: byTag("active-tail"),
    activeRoot: byTag("active-root"),
    registeredWitness: byTag("registered-witness"),
    schedulerScriptRef,
    operatorKeyHash: paymentCredential.hash,
    baseConfig: {
      lucid,
      scheduler,
      operatorKeyHash: paymentCredential.hash,
      schedulerInput,
      refreshedDatum: {
        ActiveOperator: {
          operator: paymentCredential.hash,
          start_time: 42n,
        },
      },
      validFrom,
      validTo: validFrom + 600_000n,
      schedulerSpendingScriptRef: schedulerScriptRef,
    },
  };
};

describe("scheduler refresh datum encoding", () => {
  it("encodes scheduler datums with a definite root array for deployed validators", () => {
    expect(
      encodeSchedulerDatumForChain({
        ActiveOperator: {
          operator: "aa",
          start_time: 42n,
        },
      }),
    ).toBe("d87a8241aa182a");
  });
});
