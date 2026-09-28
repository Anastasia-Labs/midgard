import { writeFileSync } from "node:fs";
import { join } from "node:path";

import { DEPLOYMENT_PROFILES } from "@al-ft/midgard-core/deployment-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { h32 } from "@al-ft/midgard-test-support/hex";
import {
  type Assets,
  calculateMinLovelaceFromUTxO,
  CML,
  credentialToRewardAddress,
  Data,
  Emulator,
  type EmulatorAccount,
  generateEmulatorAccountFromPrivateKey,
  getAddressDetails,
  Lucid,
  paymentCredentialOf,
  type TxBuilder,
  type UTxO,
  utxoToTransactionInput,
  utxoToTransactionOutput,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { createScalusEvaluator } from "@lucid-evolution/scalus-uplc";
import * as UPLC from "@lucid-evolution/uplc";
import { blake2b } from "@noble/hashes/blake2.js";
import { Effect } from "effect";
import { expect } from "vitest";

import { TEST_AVAILABILITY_PARAMETERS } from "./availability-challenge.js";
import {
  MAINNET_PROTOCOL_PARAMETERS,
  MAINNET_PROTOCOL_PARAMETERS_SOURCE,
} from "./mainnet-protocol-parameters.js";
import { loadRealMidgardContractsForTest } from "./real-midgard-contracts.js";

export const AVAILABILITY_EMULATOR_PARAMETERS = {
  ...MAINNET_PROTOCOL_PARAMETERS,
} as const;

/**
 * The deployment profile the blueprint under test is compiled for. Every
 * window below is the generated profile's value, which the validators compile
 * in; none is an SDK constant.
 */
export const AVAILABILITY_PROFILE = DEPLOYMENT_PROFILES["preprod-testing"];
export const AVAILABILITY_TIMING = Object.freeze({
  /** `OpenChallenge` needs `validTo - 1 < end_time + da_challenge_window_ms`. */
  daChallengeWindowMs: BigInt(
    AVAILABILITY_PROFILE.timing.da_challenge_window_ms,
  ),
  daSlashGraceMs: BigInt(AVAILABILITY_PROFILE.timing.da_slash_grace_ms),
  /** `BeginWithdraw` writes `unlock_at = validTo - 1 + delay`. */
  daBondWithdrawDelayMs: BigInt(
    AVAILABILITY_PROFILE.timing.da_bond_withdraw_delay_ms,
  ),
});

/**
 * The largest exact fee a DA availability builder sets is the timeout's:
 * `min(penalty, taken) + c <= da_slash_penalty + max_timeout_fee`. The ledger
 * holds `collateralPercentage` of it as collateral (G9, H1).
 */
export const AVAILABILITY_REQUIRED_COLLATERAL_LOVELACE =
  ((TEST_AVAILABILITY_PARAMETERS.da_slash_penalty_lovelace +
    TEST_AVAILABILITY_PARAMETERS.max_timeout_fee_lovelace) *
    BigInt(AVAILABILITY_EMULATOR_PARAMETERS.collateralPercentage) +
    99n) /
  100n;
/**
 * One plain-ADA collateral coin covers the largest collateral alone and
 * leaves a collateral return above min-UTxO.
 */
export const AVAILABILITY_COLLATERAL_COIN_LOVELACE =
  AVAILABILITY_REQUIRED_COLLATERAL_LOVELACE + 5_000_000n;

/**
 * P3: queue nodes carry at least this much, so an Apply or Open output that
 * grows the node datum stays above min-UTxO.
 */
export const AVAILABILITY_QUEUE_NODE_LOVELACE = 5_000_000n;

/**
 * Any amount at or above the attestation output's min-UTxO; Apply refunds it
 * whole to the rescue beneficiary. Covers the 16-tranche commitment datum.
 */
export const AVAILABILITY_ATTESTATION_OUTPUT_LOVELACE = 25_000_000n;

/** `floor + 2 * da_bond`: backs two attestations and one full slash. */
export const AVAILABILITY_DEFAULT_POOL_LOVELACE =
  TEST_AVAILABILITY_PARAMETERS.da_bond_pool_floor_lovelace +
  2n * TEST_AVAILABILITY_PARAMETERS.da_bond_lovelace;

export type AvailabilityMeasurement = {
  name: string;
  signedBytes: number;
  memory: bigint;
  steps: bigint;
  outputs: number;
  fee: bigint;
  referenceInputCount: number;
  referencedScriptBytes: number;
  uniqueReferencedScriptBytes: number;
};

export const measureAvailabilityTransaction = (
  name: string,
  cbor: string,
  references: readonly UTxO[] = [],
): AvailabilityMeasurement => {
  const transaction = CML.Transaction.from_cbor_hex(cbor);
  const redeemers = transaction.witness_set().redeemers()?.to_flat_format();
  let memory = 0n;
  let steps = 0n;
  for (let index = 0; index < (redeemers?.len() ?? 0); index += 1) {
    const units = redeemers!.get(index).ex_units();
    memory += units.mem();
    steps += units.steps();
  }
  const scripts = references.flatMap((input) =>
    input.scriptRef ? [input.scriptRef] : [],
  );
  const uniqueScripts = new Map(
    scripts.map((script) => [validatorToScriptHash(script), script]),
  );
  const result = {
    name,
    signedBytes: cbor.length / 2,
    memory,
    steps,
    outputs: transaction.body().outputs().len(),
    fee: transaction.body().fee(),
    referenceInputCount: transaction.body().reference_inputs()?.len() ?? 0,
    referencedScriptBytes: scripts.reduce(
      (total, script) => total + script.script.length / 2,
      0,
    ),
    uniqueReferencedScriptBytes: [...uniqueScripts.values()].reduce(
      (total, script) => total + script.script.length / 2,
      0,
    ),
  };
  expect(result.signedBytes, name).toBeLessThanOrEqual(16_384);
  expect(
    memory,
    `${name}: aggregate memory with 20% reserve`,
  ).toBeLessThanOrEqual(13_200_000n);
  expect(steps, `${name}: aggregate CPU with 20% reserve`).toBeLessThanOrEqual(
    8_000_000_000n,
  );
  return result;
};

type AvailabilityLayout = {
  inputs: readonly UTxO[];
  references: readonly UTxO[];
  policies: readonly string[];
};
const compare = (a: UTxO, b: UTxO) =>
  a.txHash < b.txHash
    ? -1
    : a.txHash > b.txHash
      ? 1
      : a.outputIndex - b.outputIndex;
const position = (inputs: readonly UTxO[], utxo: UTxO) => {
  const found = [...inputs]
    .sort(compare)
    .findIndex(
      (input) =>
        input.txHash === utxo.txHash && input.outputIndex === utxo.outputIndex,
    );
  if (found < 0) throw new Error("Missing authored availability input");
  return BigInt(found);
};
const index = (layout: AvailabilityLayout, utxo: UTxO) =>
  position(layout.inputs, utxo);
const refIndex = (layout: AvailabilityLayout, utxo: UTxO) =>
  position(layout.references, utxo);
const spendingInputs = (layout: AvailabilityLayout) =>
  layout.inputs.filter(
    (input) => paymentCredentialOf(input.address).type === "Script",
  );
const mintIndex = (layout: AvailabilityLayout, policy: string) =>
  BigInt(
    spendingInputs(layout).length + [...layout.policies].sort().indexOf(policy),
  );
const inline = (value: string) => ({ kind: "inline" as const, value });
const outRef = SDK.outputReferenceFromUTxO;
const sameOutRef = (a: UTxO, b: UTxO) =>
  a.txHash === b.txHash && a.outputIndex === b.outputIndex;

// ---------------------------------------------------------------------------
// Evaluation capture (H9). The fixture's Lucid evaluates with Scalus through
// this wrapper, which keeps the transaction and resolved inputs of the last
// failed evaluation so a refusal can be attributed to one redeemer and script.
// ---------------------------------------------------------------------------

type EvaluationFailure = {
  readonly sequence: number;
  readonly tx: string;
  readonly utxos: readonly UTxO[];
  /** The Scalus evaluator's message (it names no redeemer). */
  readonly message: string;
  /**
   * The same transaction re-evaluated by the Aiken machine, which names the
   * failing redeemer (`Spend[i]`, `Mint[i]`, `Reward[i]`, ...) and its trace.
   * Diagnosis only: budgets always come from Scalus.
   */
  readonly diagnosis: string;
};
let evaluationSequence = 0;
let lastEvaluationFailure: EvaluationFailure | undefined;

type LucidEvaluator = NonNullable<
  NonNullable<Parameters<typeof Lucid>[2]>["evaluator"]
>;
type EvaluatorInput = Parameters<LucidEvaluator["evaluate"]>[0];

const diagnoseWithAiken = ({
  tx,
  additionalUTxOs,
  context,
}: EvaluatorInput): string => {
  try {
    UPLC.eval_phase_two_raw(
      CML.Transaction.from_cbor_hex(tx).to_cbor_bytes(),
      additionalUTxOs.map((utxo) =>
        utxoToTransactionInput(utxo).to_cbor_bytes(),
      ),
      additionalUTxOs.map((utxo) =>
        utxoToTransactionOutput(utxo).to_cbor_bytes(),
      ),
      context.costModels.to_cbor_bytes(),
      context.protocolParameters.maxTxExSteps,
      context.protocolParameters.maxTxExMem,
      BigInt(context.slotConfig.zeroTime),
      BigInt(context.slotConfig.zeroSlot),
      context.slotConfig.slotLength,
    );
    return "aiken evaluation succeeded";
  } catch (cause) {
    return cause instanceof Error ? cause.message : String(cause);
  }
};

const capturingEvaluator = (inner: LucidEvaluator): LucidEvaluator => ({
  name: inner.name,
  evaluate: async (input) => {
    try {
      return await inner.evaluate(input);
    } catch (cause) {
      evaluationSequence += 1;
      lastEvaluationFailure = {
        sequence: evaluationSequence,
        tx: input.tx,
        utxos: input.additionalUTxOs,
        message: cause instanceof Error ? cause.message : String(cause),
        diagnosis: diagnoseWithAiken(input),
      };
      throw cause;
    }
  },
});

/** The last evaluation failure the fixture evaluator saw (diagnosis). */
export const lastAvailabilityEvaluationFailure = () => lastEvaluationFailure;

/** Mainnet (Van Rossem) costing through the capturing Scalus evaluator. */
export const createAvailabilityEmulatorLucid = (emulator: Emulator) =>
  Lucid(emulator, "Preprod", {
    evaluator: capturingEvaluator(
      createScalusEvaluator({
        protocolMajorVersion: MAINNET_PROTOCOL_PARAMETERS_SOURCE.protocolMajor,
      }),
    ),
  });

export type AvailabilityRefusalPurpose = "spend" | "mint" | "withdraw";
export type AvailabilityRefusalExpectation =
  | {
      readonly purpose: AvailabilityRefusalPurpose;
      /** A script hash or a contract name (`AvailabilityScriptNames`). */
      readonly script: string;
      /** The failing redeemer's ledger index, when the test pins it. */
      readonly index?: number;
    }
  | { readonly trace: RegExp };

export type AvailabilityRefusal = {
  readonly purpose: AvailabilityRefusalPurpose | "other";
  readonly index: number;
  readonly scriptHash: string | undefined;
  readonly message: string;
};

const AIKEN_TAG_PURPOSE: Readonly<
  Record<string, AvailabilityRefusalPurpose | "other">
> = {
  Spend: "spend",
  Mint: "mint",
  Withdraw: "withdraw",
  Publish: "other",
  Vote: "other",
  Propose: "other",
};

/**
 * The failing redeemer as the Aiken machine reports it
 * (`failed script execution\n  Mint[0] ...`): its tag and ledger index.
 */
export const parseAvailabilityEvaluationFailure = (
  diagnosis: string,
): { purpose: AvailabilityRefusalPurpose | "other"; index: number } => {
  const match = /\b(Spend|Mint|Withdraw|Publish|Vote|Propose)\[(\d+)\]/.exec(
    diagnosis,
  );
  if (!match)
    throw new Error(
      `Cannot attribute the evaluation failure to a redeemer: ${diagnosis}`,
    );
  return {
    purpose: AIKEN_TAG_PURPOSE[match[1]!] ?? "other",
    index: Number(match[2]),
  };
};

/**
 * Maps a redeemer (purpose, ledger index) of `txCbor` to the script it runs:
 * a spend to its sorted input's payment script, a mint to its sorted policy,
 * a withdrawal to its sorted reward account's script.
 */
export const availabilityRedeemerScript = (
  txCbor: string,
  utxos: readonly UTxO[],
  purpose: AvailabilityRefusalPurpose | "other",
  redeemerIndex: number,
): string | undefined => {
  const body = CML.Transaction.from_cbor_hex(txCbor).body();
  if (purpose === "spend") {
    const inputs = body.inputs();
    const refs = Array.from({ length: inputs.len() }, (_, i) => ({
      txHash: inputs.get(i).transaction_id().to_hex(),
      outputIndex: Number(inputs.get(i).index()),
    })).sort((a, b) =>
      a.txHash < b.txHash
        ? -1
        : a.txHash > b.txHash
          ? 1
          : a.outputIndex - b.outputIndex,
    );
    const spent = refs[redeemerIndex];
    const utxo = utxos.find(
      (u) =>
        spent !== undefined &&
        u.txHash === spent.txHash &&
        u.outputIndex === spent.outputIndex,
    );
    const credential = utxo
      ? getAddressDetails(utxo.address).paymentCredential
      : undefined;
    return credential?.type === "Script" ? credential.hash : undefined;
  }
  if (purpose === "mint") {
    const policies = body.mint()?.keys();
    return Array.from({ length: policies?.len() ?? 0 }, (_, i) =>
      policies!.get(i).to_hex(),
    ).sort()[redeemerIndex];
  }
  if (purpose === "withdraw") {
    const withdrawals = body.withdrawals();
    const accounts = withdrawals?.keys();
    const credentials = Array.from({ length: accounts?.len() ?? 0 }, (_, i) => {
      const account = accounts!.get(i);
      return {
        bytes: account.to_address().to_hex(),
        payment: account.payment(),
      };
    }).sort((a, b) => (a.bytes < b.bytes ? -1 : a.bytes > b.bytes ? 1 : 0));
    const account = credentials[redeemerIndex];
    return account?.payment.as_script()?.to_hex();
  }
  return undefined;
};

let registeredScriptNames: Readonly<Record<string, string>> = {};

/**
 * H9: asserts `attempt` is refused by local evaluation, and that the failing
 * redeemer runs the expected script under the expected purpose (or that the
 * evaluator's message matches `trace`). A refusal elsewhere, or no refusal,
 * fails the test. `attempt` is a promise the caller already started (an SDK
 * build, or `TxBuilder.complete`), or a hand-built `TxBuilder` this helper
 * completes without coin selection.
 */
export const assertAvailabilityRefusal = async (
  attempt: Promise<unknown> | TxBuilder,
  expected: AvailabilityRefusalExpectation,
  /** Contract names to script hashes; defaults to the latest fixture's. */
  names: Readonly<Record<string, string>> = registeredScriptNames,
): Promise<AvailabilityRefusal> => {
  const before = evaluationSequence;
  const promise =
    attempt instanceof Promise
      ? attempt
      : attempt.complete({ coinSelection: false, localUPLCEval: true });
  let rejection: unknown;
  try {
    await promise;
  } catch (cause) {
    rejection = cause;
  }
  if (rejection === undefined)
    throw new Error(
      `Expected an evaluation refusal (${describeExpectation(expected)}), but the transaction completed`,
    );
  const failure = lastEvaluationFailure;
  if (failure === undefined || failure.sequence <= before)
    throw new Error(
      `Expected an evaluation refusal (${describeExpectation(expected)}), but the build failed before evaluation: ${rejection instanceof Error ? rejection.message : String(rejection)}`,
    );
  const located = parseAvailabilityEvaluationFailure(failure.diagnosis);
  const scriptHash = availabilityRedeemerScript(
    failure.tx,
    failure.utxos,
    located.purpose,
    located.index,
  );
  const refusal = { ...located, scriptHash, message: failure.diagnosis };
  if ("trace" in expected) {
    expect(`${failure.diagnosis}\n${failure.message}`).toMatch(expected.trace);
    return refusal;
  }
  const expectedHash = names[expected.script] ?? expected.script;
  const label = (hash: string | undefined) => {
    const name = Object.entries(names).find(([, h]) => h === hash)?.[0];
    return `${hash ?? "unknown"}${name ? ` (${name})` : ""}`;
  };
  expect(
    { purpose: located.purpose, script: label(scriptHash) },
    `refusal must come from the expected check; evaluator said: ${failure.diagnosis}`,
  ).toEqual({ purpose: expected.purpose, script: label(expectedHash) });
  if (expected.index !== undefined) expect(located.index).toBe(expected.index);
  return refusal;
};

const describeExpectation = (expected: AvailabilityRefusalExpectation) =>
  "trace" in expected
    ? `trace ${String(expected.trace)}`
    : `${expected.purpose} ${expected.script}${expected.index === undefined ? "" : `[${expected.index}]`}`;

// ---------------------------------------------------------------------------
// Fixture
// ---------------------------------------------------------------------------

export type AvailabilityFixture = Awaited<
  ReturnType<typeof createAvailabilityFixture>
>;
export type OpenAvailability = Awaited<ReturnType<typeof openAvailability>>;
export type AttestedAvailability = Awaited<
  ReturnType<typeof attestAvailability>
>;

export const reportAvailabilityScenario = (
  name: string,
  fixture: AvailabilityFixture,
) => {
  const worst = (
    key: "signedBytes" | "memory" | "steps" | "referencedScriptBytes",
  ) =>
    fixture.measurements.reduce((left, right) =>
      left[key] >= right[key] ? left : right,
    );
  const summary = {
    scenario: name,
    transactionCount: fixture.measurements.length,
    largestTransaction: worst("signedBytes"),
    highestMemory: worst("memory"),
    highestCpu: worst("steps"),
    mostReferencedScriptBytes: worst("referencedScriptBytes"),
  };
  const bigintJson = (_key: string, value: unknown) =>
    typeof value === "bigint" ? value.toString() : value;
  console.info("availability mainnet fit", JSON.stringify(summary, bigintJson));
  const reportDirectory = process.env.MIDGARD_AVAILABILITY_FIT_REPORT_DIR;
  if (reportDirectory)
    writeFileSync(
      join(reportDirectory, `${name}.json`),
      JSON.stringify(
        { summary, measurements: fixture.measurements },
        bigintJson,
        2,
      ) + "\n",
    );
};

/** The reference-script roles the availability, DA and pool flows read. */
export const availabilityReferenceScriptTargets = (
  contracts: SDK.MidgardValidators,
): readonly SDK.ReferenceScriptTarget[] => [
  {
    name: "availability-challenge spending",
    script: contracts.availabilityChallenge.spendingScript,
  },
  {
    name: "availability-challenge minting",
    script: contracts.availabilityChallenge.mintingScript,
  },
  ...(["open", "settle", "close", "timeout"] as const).map((arm) => ({
    name: `availability-challenge ${arm} withdrawal`,
    script: contracts.availabilityChallenge.yields[arm].withdrawalScript,
  })),
  {
    name: "da-attestation spending",
    script: contracts.daAttestation.spendingScript,
  },
  {
    name: "da-attestation minting",
    script: contracts.daAttestation.mintingScript,
  },
  {
    name: "da-bond-pool spending",
    script: contracts.daBondPool.spendingScript,
  },
  {
    name: "da-bond-pool minting",
    script: contracts.daBondPool.mintingScript,
  },
  { name: "state-queue spending", script: contracts.stateQueue.spendingScript },
  { name: "state-queue minting", script: contracts.stateQueue.mintingScript },
  {
    name: "state-queue unavailable-timeout withdrawal",
    script: contracts.stateQueue.yields.unavailableTimeout.withdrawalScript,
  },
  {
    name: "state-queue merge withdrawal",
    script: contracts.stateQueue.yields.merge.withdrawalScript,
  },
  {
    name: "correction-lock spending",
    script: contracts.correctionLock.spendingScript,
  },
];

/** Script hashes by contract name, for `assertAvailabilityRefusal`. */
const availabilityScriptNames = (contracts: SDK.MidgardValidators) =>
  Object.freeze({
    "availability-challenge spending":
      contracts.availabilityChallenge.spendingScriptHash,
    "availability-challenge minting": contracts.availabilityChallenge.policyId,
    "availability-challenge open withdrawal":
      contracts.availabilityChallenge.yields.open.withdrawalScriptHash,
    "availability-challenge settle withdrawal":
      contracts.availabilityChallenge.yields.settle.withdrawalScriptHash,
    "availability-challenge close withdrawal":
      contracts.availabilityChallenge.yields.close.withdrawalScriptHash,
    "availability-challenge timeout withdrawal":
      contracts.availabilityChallenge.yields.timeout.withdrawalScriptHash,
    "da-attestation spending": contracts.daAttestation.spendingScriptHash,
    "da-attestation minting": contracts.daAttestation.policyId,
    "da-bond-pool": contracts.daBondPool.policyId,
    "state-queue spending": contracts.stateQueue.spendingScriptHash,
    "state-queue minting": contracts.stateQueue.policyId,
    "state-queue unavailable-timeout withdrawal":
      contracts.stateQueue.yields.unavailableTimeout.withdrawalScriptHash,
    "state-queue merge withdrawal":
      contracts.stateQueue.yields.merge.withdrawalScriptHash,
    "correction-lock spending": contracts.correctionLock.spendingScriptHash,
  } as const);
export type AvailabilityScriptName = keyof ReturnType<
  typeof availabilityScriptNames
>;

export type AvailabilityFixtureOptions = {
  /**
   * The DA bond pool's genesis state: an inline datum (default `Bonded`) and
   * the pool NFT at `Script(pool policy)` with `lovelace` (default
   * `floor + 2 * da_bond`). `false` seeds no pool and leaves the hub one-shot
   * out-reference unspent, so `initPoolReal` can run the real `InitPool`.
   */
  readonly seedPool?:
    | false
    | {
        readonly lovelace?: bigint;
        readonly datum?: SDK.DaBondPoolDatum;
      };
};

/**
 * The genesis fixture represents an already deployed protocol and committed
 * block. No attestation, challenge, tranche, carrier or terminal asset is
 * seeded: every availability state below is produced by an evaluated ledger
 * transaction. The pooled DA bond is seeded at genesis unless
 * `options.seedPool` is `false`.
 *
 * Genesis output 0 is the hub one-shot out-reference (`"00" * 32 #0`) the
 * contracts are parameterised with; it belongs to `oneShotHolder` and is never
 * spent by any other helper.
 */
export const createAvailabilityFixture = async (
  payloadBytes = 14_021,
  descendantCount = 0,
  /**
   * Places the target header's end time this far after fixture start. Setup
   * advances the emulator by several minutes, which can exceed the selected
   * profile's DA attestation timeout; a lead keeps apply's deadline ahead.
   */
  headerEndTimeLeadMs = 0,
  options: AvailabilityFixtureOptions = {},
) => {
  const parameters = TEST_AVAILABILITY_PARAMETERS;
  const oneShotHolder = generateEmulatorAccountFromPrivateKey({
    lovelace: 2_000_000_000n,
  });
  const responder = generateEmulatorAccountFromPrivateKey({
    lovelace: 100_000_000_000n,
  });
  const challenger = generateEmulatorAccountFromPrivateKey({
    lovelace: 50_000_000_000n,
  });
  const publisher = generateEmulatorAccountFromPrivateKey({
    lovelace: 100_000_000_000n,
  });
  const preliminary = new Emulator(
    [publisher],
    AVAILABILITY_EMULATOR_PARAMETERS,
  );
  const preliminaryLucid = await createAvailabilityEmulatorLucid(preliminary);
  preliminaryLucid.selectWallet.fromPrivateKey(publisher.privateKey);
  const authPolicy = await SDK.createReferenceScriptAuthPolicy(
    preliminaryLucid,
    preliminary.now(),
  );
  const hubOneShot = { txHash: "00".repeat(32), outputIndex: 0 };
  const contracts = await loadRealMidgardContractsForTest(
    hubOneShot,
    authPolicy,
  );
  const scriptNames = availabilityScriptNames(contracts);
  registeredScriptNames = scriptNames;
  const now = preliminary.now();
  const challengerKey = paymentCredentialOf(challenger.address).hash;
  const responderKey = paymentCredentialOf(responder.address).hash;
  const committeeKeys = [
    CML.PrivateKey.generate_ed25519(),
    CML.PrivateKey.generate_ed25519(),
  ].sort((a, b) =>
    Buffer.compare(
      Buffer.from(a.to_public().to_raw_bytes()),
      Buffer.from(b.to_public().to_raw_bytes()),
    ),
  );
  const committee = committeeKeys
    .map((key) => Buffer.from(key.to_public().to_raw_bytes()).toString("hex"))
    .join("");
  const daParamsDatum: SDK.DaParamsDatum = {
    committee,
    committee_signers_hash: Buffer.from(
      blake2b(Buffer.from(committee, "hex"), { dkLen: 32 }),
    ).toString("hex"),
    da_threshold: 2n,
    owners: [challengerKey, responderKey].sort(),
    update_threshold: 2n,
  };
  const stateQueueNode: SDK.StateQueueNode = {
    proven_fraud: null,
    header: {
      prevUtxosRoot: h32(0x01),
      utxosRoot: h32(0x02),
      withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      ...SDK.EMPTY_HEADER_TRANSITION_COMMITMENTS,
      startTime: BigInt(now - 1_000),
      endTime: BigInt(now + headerEndTimeLeadMs),
      blockSlot: 0n,
      expectedNetworkId: 0n,
      minFeeA: 44n,
      minFeeB: 155381n,
      prevHeaderHash: SDK.GENESIS_HEADER_HASH,
      operatorVkey: responderKey,
      protocolVersion: 1n,
    },
    da_attestation: SDK.NO_DA_ATTESTATION,
  };
  await Effect.runPromise(
    SDK.validateHeaderTransitionCommitmentsProgram(stateQueueNode.header),
  );
  const headerHash = await Effect.runPromise(
    SDK.hashBlockHeader(stateQueueNode.header),
  );
  const queueDatum: SDK.LinkedListNodeView = {
    key: { Key: { key: headerHash } },
    next: "Empty",
    data: SDK.castStateQueueNodeToData(
      stateQueueNode,
    ) as SDK.LinkedListNodeView["data"],
  };
  const descendants: { hash: string; datum: SDK.LinkedListNodeView }[] = [];
  let previousHash = headerHash;
  for (let i = 0; i < descendantCount; i++) {
    const node: SDK.StateQueueNode = {
      ...stateQueueNode,
      header: {
        ...stateQueueNode.header,
        prevHeaderHash: previousHash,
        blockSlot: BigInt(i + 1),
      },
    };
    const hash = await Effect.runPromise(SDK.hashBlockHeader(node.header));
    descendants.push({
      hash,
      datum: {
        key: { Key: { key: hash } },
        next: "Empty",
        data: SDK.castStateQueueNodeToData(
          node,
        ) as SDK.LinkedListNodeView["data"],
      },
    });
    previousHash = hash;
  }
  if (descendants.length > 0)
    queueDatum.next = { Key: { key: descendants[0]!.hash } };
  for (let i = 0; i < descendants.length - 1; i++)
    descendants[i]!.datum.next = { Key: { key: descendants[i + 1]!.hash } };
  const rootDatum: SDK.LinkedListNodeView = {
    key: "Empty",
    next: { Key: { key: headerHash } },
    data: SDK.castConfirmedStateToData(
      SDK.makeGenesisConfirmedState(BigInt(now - 2_000)),
    ) as SDK.LinkedListNodeView["data"],
  };
  const hubDatum = await Effect.runPromise(SDK.makeHubOracleDatum(contracts));
  const queueUnit =
    contracts.stateQueue.policyId +
    SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
    headerHash;
  const rootUnit =
    contracts.stateQueue.policyId + SDK.STATE_QUEUE_ROOT_ASSET_NAME;
  const hubUnit = contracts.hubOracle.policyId + SDK.HUB_ORACLE_ASSET_NAME;
  const paramsUnit =
    contracts.daParamsGovernor.policyId + SDK.DA_PARAMS_ASSET_NAME;
  const lockUnit = SDK.correctionLockUnit(contracts.hubOracle.policyId);
  const poolUnit = SDK.daBondPoolUnit(contracts.daBondPool.policyId);
  const genesis = (
    address: string,
    assets: Assets,
    datum?: string,
  ): EmulatorAccount => ({
    seedPhrase: "",
    privateKey: "",
    address,
    assets,
    ...(datum === undefined ? {} : { outputData: { inline: datum } }),
  });
  const seededPool =
    options.seedPool === false
      ? undefined
      : {
          lovelace:
            options.seedPool?.lovelace ?? AVAILABILITY_DEFAULT_POOL_LOVELACE,
          datum: options.seedPool?.datum ?? ("Bonded" as const),
        };
  const emulator = new Emulator(
    [
      // Output 0 is the hub one-shot out-reference; see the doc comment.
      oneShotHolder,
      responder,
      challenger,
      publisher,
      genesis(responder.address, {
        lovelace: AVAILABILITY_COLLATERAL_COIN_LOVELACE,
      }),
      genesis(challenger.address, {
        lovelace: AVAILABILITY_COLLATERAL_COIN_LOVELACE,
      }),
      genesis(
        contracts.hubOracle.spendingScriptAddress,
        { lovelace: 20_000_000n, [hubUnit]: 1n },
        Data.to(hubDatum, SDK.HubOracleDatum),
      ),
      genesis(
        contracts.daParamsGovernor.spendingScriptAddress,
        { lovelace: 3_000_000n, [paramsUnit]: 1n },
        Data.to(daParamsDatum, SDK.DaParamsDatum),
      ),
      genesis(
        contracts.stateQueue.spendingScriptAddress,
        { lovelace: AVAILABILITY_QUEUE_NODE_LOVELACE, [queueUnit]: 1n },
        SDK.encodeLinkedListNodeView(queueDatum),
      ),
      genesis(
        contracts.stateQueue.spendingScriptAddress,
        { lovelace: AVAILABILITY_QUEUE_NODE_LOVELACE, [rootUnit]: 1n },
        SDK.encodeLinkedListNodeView(rootDatum),
      ),
      ...descendants.map(({ hash, datum }) =>
        genesis(
          contracts.stateQueue.spendingScriptAddress,
          {
            lovelace: AVAILABILITY_QUEUE_NODE_LOVELACE,
            [contracts.stateQueue.policyId +
            SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
            hash]: 1n,
          },
          SDK.encodeLinkedListNodeView(datum),
        ),
      ),
      genesis(
        contracts.correctionLock.spendingScriptAddress,
        { lovelace: 3_000_000n, [lockUnit]: 1n },
        Data.to("Idle", SDK.CorrectionLockDatum),
      ),
      ...(seededPool === undefined
        ? []
        : [
            genesis(
              contracts.daBondPool.spendingScriptAddress,
              { lovelace: seededPool.lovelace, [poolUnit]: 1n },
              SDK.encodeDaBondPoolDatum(seededPool.datum),
            ),
          ]),
    ],
    AVAILABILITY_EMULATOR_PARAMETERS,
  );
  // Created at emulator genesis, so zeroTime is genesis time and zeroSlot 0:
  // never create another Lucid mid-test (its zeroTime would be "now").
  const lucid = await createAvailabilityEmulatorLucid(emulator);
  const publishingLucid = await createAvailabilityEmulatorLucid(emulator);
  lucid.selectWallet.fromPrivateKey(responder.privateKey);
  publishingLucid.selectWallet.fromPrivateKey(publisher.privateKey);
  const measurements: AvailabilityMeasurement[] = [];
  const submit = async (
    name: string,
    builder: TxBuilder,
    coinSelection = false,
    /** Private keys that sign beside the selected wallet. */
    extraSigners: readonly string[] = [],
  ) => {
    const unsigned = await builder
      .complete({ localUPLCEval: true, coinSelection })
      .catch((cause: unknown) => {
        throw new Error(`Availability stage ${name}: ${String(cause)}`, {
          cause,
        });
      });
    let signing = unsigned.sign.withWallet();
    for (const key of extraSigners) signing = signing.sign.withPrivateKey(key);
    const signed = await signing.complete();
    const referenceInputs = CML.Transaction.from_cbor_hex(signed.toCBOR())
      .body()
      .reference_inputs();
    const refs = await lucid.utxosByOutRef(
      Array.from({ length: referenceInputs?.len() ?? 0 }, (_, i) => {
        const input = referenceInputs!.get(i);
        return {
          txHash: input.transaction_id().to_hex(),
          outputIndex: Number(input.index()),
        };
      }),
    );
    measurements.push(
      measureAvailabilityTransaction(name, signed.toCBOR(), refs),
    );
    const hash = await signed.submit();
    emulator.awaitBlock(1);
    return lucid.utxosByOutRef(
      Array.from(
        {
          length: CML.Transaction.from_cbor_hex(signed.toCBOR())
            .body()
            .outputs()
            .len(),
        },
        (_, outputIndex) => ({ txHash: hash, outputIndex }),
      ),
    );
  };
  const references = new Map<string, UTxO>();
  for (const target of availabilityReferenceScriptTargets(contracts)) {
    const { tx, layout } = await Effect.runPromise(
      SDK.completeReferenceScriptPublicationTxProgram({
        lucid: publishingLucid,
        selectedFundingInputs: SDK.selectReferenceScriptFundingUtxos(
          await publishingLucid.wallet().getUtxos(),
          SDK.referenceScriptPublicationFundingTarget(1),
        ),
        walletAddress: publisher.address,
        referenceScriptsAddress: publisher.address,
        missingTargets: [target],
        authPolicy,
      }),
    );
    const signed = await tx.sign.withWallet().complete();
    const measurement = measureAvailabilityTransaction(
      `publish ${target.name}`,
      signed.toCBOR(),
    );
    expect(measurement.signedBytes).toBeLessThanOrEqual(15_872);
    measurements.push(measurement);
    const hash = await signed.submit();
    emulator.awaitBlock(1);
    const local = layout.localReferenceOutputs.get(target.name);
    if (!local) throw new Error(`Missing published role ${target.name}`);
    const [utxo] = await lucid.utxosByOutRef([
      { txHash: hash, outputIndex: local.outputIndex },
    ]);
    if (!utxo) throw new Error(`Missing published reference ${target.name}`);
    references.set(target.name, utxo);
  }
  const reference = (name: string): UTxO => {
    const utxo = references.get(name);
    if (!utxo) throw new Error(`Unavailable reference ${name}`);
    return utxo;
  };
  let registrations = lucid.newTx();
  const rewardAddresses = [
    ...Object.values(contracts.availabilityChallenge.yields),
    contracts.stateQueue.yields.unavailableTimeout,
    contracts.stateQueue.yields.merge,
  ].map(({ withdrawalScript }) =>
    SDK.scriptRewardAddress("Preprod", withdrawalScript),
  );
  for (const address of rewardAddresses)
    registrations = registrations.register.Stake(address);
  await submit("register availability reward credentials", registrations, true);
  for (const address of rewardAddresses)
    expect((await lucid.rewardAccountAt(address)).registered).toBe(true);
  const [hubOracleRefInput] = await lucid.utxosAtWithUnit(
    contracts.hubOracle.spendingScriptAddress,
    hubUnit,
  );
  const [daParamsUtxo] = await lucid.utxosAtWithUnit(
    contracts.daParamsGovernor.spendingScriptAddress,
    paramsUnit,
  );
  const [queueUtxo] = await lucid.utxosAtWithUnit(
    contracts.stateQueue.spendingScriptAddress,
    queueUnit,
  );
  const [rootUtxo] = await lucid.utxosAtWithUnit(
    contracts.stateQueue.spendingScriptAddress,
    rootUnit,
  );
  const [correctionLockUtxo] = await lucid.utxosAtWithUnit(
    contracts.correctionLock.spendingScriptAddress,
    lockUnit,
  );
  const [hubOneShotUtxo] = await lucid.utxosByOutRef([hubOneShot]);
  if (
    !hubOracleRefInput ||
    !daParamsUtxo ||
    !queueUtxo ||
    !rootUtxo ||
    !correctionLockUtxo ||
    !hubOneShotUtxo
  )
    throw new Error("Incomplete genesis fixture");
  const payload = Uint8Array.from(
    { length: payloadBytes },
    (_, i) => (i * 17 + 3) % 256,
  );
  const commitment = SDK.buildDaAvailabilityCommitment({
    deploymentIdentity: contracts.hubOracle.policyId,
    headerHash,
    payload,
    responseGeometry: SDK.availabilityResponseGeometry({
      chunkByteLength: Number(parameters.response_geometry.chunk_byte_length),
      trancheByteLength: Number(
        parameters.response_geometry.tranche_byte_length,
      ),
      maxTrancheCount: Number(parameters.response_geometry.max_tranche_count),
    }),
  });
  const target: SDK.DaAttestationStateQueueTarget = {
    headerHash,
    stateQueueNode,
    stateQueueUtxo: {
      utxo: queueUtxo,
      datum: queueDatum,
      assetName: SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
    },
  };
  const daReferences: SDK.DaAttestationReferenceScripts = {
    daAttestationMinting: reference("da-attestation minting"),
    daAttestationSpending: reference("da-attestation spending"),
    stateQueueMinting: reference("state-queue minting"),
    stateQueueSpending: reference("state-queue spending"),
  };
  const poolReferences = {
    daBondPoolMinting: reference("da-bond-pool minting"),
    daBondPoolSpending: reference("da-bond-pool spending"),
  };

  // --- pool helpers --------------------------------------------------------

  /** The one live pool UTxO (fails closed when absent or ambiguous). */
  const getPool = async (): Promise<UTxO> =>
    (
      await SDK.fetchDaBondPool(lucid, {
        policyId: contracts.daBondPool.policyId,
        address: contracts.daBondPool.spendingScriptAddress,
      })
    ).utxo;
  /** Runs `body` with `privateKey`'s wallet selected, then restores `after`. */
  const asWallet = async <A>(
    privateKey: string,
    body: () => Promise<A>,
    after: string = responder.privateKey,
  ): Promise<A> => {
    lucid.selectWallet.fromPrivateKey(privateKey);
    try {
      return await body();
    } finally {
      lucid.selectWallet.fromPrivateKey(after);
    }
  };
  const poolOutput = (outputs: readonly UTxO[]) => {
    const pool = outputs.find((u) => u.assets[poolUnit] === 1n);
    if (!pool) throw new Error("Transaction produced no pool output");
    return pool;
  };
  /** Advances the emulator clock to at least `ms` (one-second slots). */
  const advanceToMs = (ms: bigint | number) => {
    const deficit = Number(ms) - emulator.now();
    if (deficit > 0) emulator.awaitSlot(Math.ceil(deficit / 1_000));
  };
  const quorum = {
    signerKeyHashes: daParamsDatum.owners,
    // The owners are the challenger and the responder: the responder's
    // wallet signs, the challenger's key signs beside it.
    extraSigners: [challenger.privateKey],
  };
  /**
   * The real `InitPool`: spends the hub one-shot out-reference (the pool's
   * `init_ref`) and mints the pool NFT to `Script(pool policy)` with a
   * `Bonded` datum. Only meaningful in a `seedPool: false` fixture.
   */
  const initPoolReal = async (
    lovelace: bigint = AVAILABILITY_DEFAULT_POOL_LOVELACE,
  ): Promise<UTxO> =>
    asWallet(oneShotHolder.privateKey, async () => {
      const tx = await Effect.runPromise(
        SDK.buildInitDaBondPoolTxProgram(lucid, {
          poolValidator: contracts.daBondPool,
          parameters,
          initUtxo: hubOneShotUtxo,
          lovelace,
          referenceScripts: {
            daBondPoolMinting: poolReferences.daBondPoolMinting,
          },
        }),
      );
      return poolOutput(await submit("init DA bond pool", tx, true));
    });
  /** `TopUp`: the selected wallet adds `amount` to the pool. */
  const topUpPool = async (
    amount: bigint,
    options: { skipMinimumPrecheck?: true } = {},
  ): Promise<UTxO> => {
    const tx = await Effect.runPromise(
      SDK.buildTopUpDaBondPoolTxProgram(lucid, {
        poolValidator: contracts.daBondPool,
        parameters,
        pool: { utxo: await getPool() },
        amount,
        referenceScripts: {
          daBondPoolSpending: poolReferences.daBondPoolSpending,
        },
        ...options,
      }),
    );
    return poolOutput(await submit(`top up DA bond pool ${amount}`, tx, true));
  };
  const quorumConfig = async () => ({
    poolValidator: contracts.daBondPool,
    parameters,
    pool: { utxo: await getPool() },
    daParamsUtxo,
    signerKeyHashes: quorum.signerKeyHashes,
    referenceScripts: {
      daBondPoolSpending: poolReferences.daBondPoolSpending,
    },
  });
  /**
   * `BeginWithdraw` under the owner quorum; the datum's `unlock_at` is the
   * slot-aligned `validTo - 1 + da_bond_withdraw_delay_ms`.
   */
  const beginPoolWithdraw = async (
    validity: { validFrom?: bigint; validTo?: bigint } = {},
  ): Promise<{ pool: UTxO; unlockAt: bigint }> =>
    asWallet(responder.privateKey, async () => {
      const validFrom = validity.validFrom ?? BigInt(emulator.now());
      const tx = await Effect.runPromise(
        SDK.buildBeginDaBondPoolWithdrawTxProgram(lucid, {
          ...(await quorumConfig()),
          withdrawDelayMs: AVAILABILITY_TIMING.daBondWithdrawDelayMs,
          validity: {
            validFrom,
            validTo: validity.validTo ?? validFrom + 60_000n,
          },
        }),
      );
      const pool = poolOutput(
        await submit(
          "begin DA bond pool withdraw",
          tx,
          true,
          quorum.extraSigners,
        ),
      );
      const datum = SDK.parseDaBondPoolDatumCbor(pool.datum!);
      if (datum === "Bonded") throw new Error("BeginWithdraw left pool Bonded");
      return { pool, unlockAt: datum.Withdrawing.unlock_at };
    });
  const cancelPoolWithdraw = async (): Promise<UTxO> =>
    asWallet(responder.privateKey, async () => {
      const tx = await Effect.runPromise(
        SDK.buildCancelDaBondPoolWithdrawTxProgram(lucid, await quorumConfig()),
      );
      return poolOutput(
        await submit(
          "cancel DA bond pool withdraw",
          tx,
          true,
          quorum.extraSigners,
        ),
      );
    });
  /**
   * `CompleteWithdraw { amount }` at or after `unlock_at`; pays `amount` to
   * `destination` (default the responder).
   */
  const completePoolWithdraw = async (
    amount: bigint,
    options: {
      destination?: string;
      validFrom?: bigint;
      skipUnlockPrecheck?: true;
    } = {},
  ): Promise<UTxO> =>
    asWallet(responder.privateKey, async () => {
      const tx = await Effect.runPromise(
        SDK.buildCompleteDaBondPoolWithdrawTxProgram(lucid, {
          ...(await quorumConfig()),
          amount,
          destination: options.destination ?? responder.address,
          validity: { validFrom: options.validFrom ?? BigInt(emulator.now()) },
          ...(options.skipUnlockPrecheck
            ? { skipUnlockPrecheck: true as const }
            : {}),
        }),
      );
      return poolOutput(
        await submit(
          `complete DA bond pool withdraw ${amount}`,
          tx,
          true,
          quorum.extraSigners,
        ),
      );
    });
  /**
   * Plain-ADA collateral coins of the selected wallet, none of `exclude`.
   * Each covers the largest DA availability collateral alone.
   */
  const collateralInputs = async (
    exclude: readonly UTxO[] = [],
  ): Promise<UTxO[]> => {
    const coins = (await lucid.wallet().getUtxos()).filter(
      (u) =>
        u.assets.lovelace === AVAILABILITY_COLLATERAL_COIN_LOVELACE &&
        Object.keys(u.assets).length === 1 &&
        !u.datum &&
        !u.datumHash &&
        !u.scriptRef &&
        !exclude.some((e) => sameOutRef(e, u)),
    );
    if (coins.length === 0)
      throw new Error(
        `The selected wallet holds no ${AVAILABILITY_COLLATERAL_COIN_LOVELACE}-lovelace collateral coin`,
      );
    return coins;
  };

  return {
    emulator,
    lucid,
    contracts,
    parameters,
    timing: AVAILABILITY_TIMING,
    scriptNames,
    authPolicy,
    oneShotHolder,
    hubOneShot,
    responder,
    challenger,
    challengerKey,
    responderKey,
    committeeKeys,
    daParamsDatum,
    daParamsUtxo,
    hubOracleRefInput,
    correctionLockUtxo,
    rootUtxo,
    rootDatum,
    rootUnit,
    queueUnit,
    poolUnit,
    target,
    payload,
    commitment,
    daReferences,
    poolReferences,
    reference,
    measurements,
    submit,
    getPool,
    initPoolReal,
    topUpPool,
    beginPoolWithdraw,
    cancelPoolWithdraw,
    completePoolWithdraw,
    advanceToMs,
    collateralInputs,
  };
};

/** The SDK challenge builders' deployment view of the fixture. */
export const availabilityDeployment = (
  f: AvailabilityFixture,
): SDK.DaAvailabilityDeployment => {
  const names = [
    "availability-challenge spending",
    "availability-challenge minting",
    ...(["open", "settle", "close", "timeout"] as const).map(
      (arm) => `availability-challenge ${arm} withdrawal`,
    ),
    "state-queue spending",
    "state-queue minting",
    "state-queue unavailable-timeout withdrawal",
    "correction-lock spending",
    "da-bond-pool spending",
  ];
  return {
    contracts: f.contracts,
    hubOraclePolicyId: f.contracts.hubOracle.policyId,
    referenceScriptAuthPolicyId: f.authPolicy.policyId,
    parameters: f.parameters,
    referenceScripts: Object.fromEntries(
      names.map((name) => [name, f.reference(name)]),
    ),
    hubOracleRefInput: f.hubOracleRefInput,
  };
};

/**
 * Attests the fixture's block: init, threshold signatures, then Apply, which
 * references the pool and writes `Attested{commitment_hash}`. Returns the
 * attested queue node and the full commitment an Open needs.
 */
export const attestAvailability = async (
  f: AvailabilityFixture,
  options: { refuseCommitmentPreimageMismatch?: boolean } = {},
) => {
  const { lucid, contracts } = f;
  lucid.selectWallet.fromPrivateKey(f.responder.privateKey);
  const init = await Effect.runPromise(
    SDK.incompleteInitDaAttestationTxProgram(lucid, contracts, {
      daParamsUtxo: f.daParamsUtxo,
      daParamsDatum: f.daParamsDatum,
      target: f.target,
      referenceScripts: f.daReferences,
      attestationOutputLovelace: AVAILABILITY_ATTESTATION_OUTPUT_LOVELACE,
      rescueBeneficiary: await Effect.runPromise(
        SDK.addressDataFromBech32(f.responder.address),
      ),
      availabilityCommitment: f.commitment,
    }),
  );
  await f.submit("attestation init", init, true);
  const attestationUnit = SDK.daAttestationUnit(
    contracts.daAttestation,
    f.target.headerHash,
  );
  const getAttestation = async (): Promise<SDK.DaAttestationUtxo> => {
    const [utxo] = await lucid.utxosAtWithUnit(
      contracts.daAttestation.spendingScriptAddress,
      attestationUnit,
    );
    if (!utxo?.datum) throw new Error("Missing attestation");
    return { utxo, datum: Data.from(utxo.datum, SDK.DaAttestationDatum) };
  };
  const message = SDK.daAvailabilityAttestationMessage(f.commitment);
  const add = await Effect.runPromise(
    SDK.incompleteAddDaAttestationSignaturesTxProgram(lucid, contracts, {
      daParamsUtxo: f.daParamsUtxo,
      daParamsDatum: f.daParamsDatum,
      attestation: await getAttestation(),
      witnesses: f.committeeKeys.map((key, signerIndex) => ({
        signerIndex,
        signatureHex: Buffer.from(key.sign(message).to_raw_bytes()).toString(
          "hex",
        ),
      })),
      referenceScripts: f.daReferences,
    }),
  );
  await f.submit("attestation threshold signatures", add, true);
  const threshold = await getAttestation();
  const applyConfig = {
    daParamsUtxo: f.daParamsUtxo,
    daParamsDatum: f.daParamsDatum,
    attestation: threshold,
    target: f.target,
    referenceScripts: f.daReferences,
    availabilityParameters: f.parameters,
    validityRange: {
      validFrom: BigInt(f.emulator.now()),
      validTo: BigInt(f.emulator.now() + 60_000),
    },
  };
  if (options.refuseCommitmentPreimageMismatch) {
    // The consumed attestation carries the committee-signed commitment. A
    // builder fed another commitment writes Attested{hash(other)}, which is
    // not the hash of the preimage the chain holds: Apply must refuse.
    const [first, ...rest] =
      threshold.datum.availability_commitment.tranche_descriptors;
    const substituted = await Effect.runPromise(
      SDK.incompleteApplyDaAttestationToStateQueueTxProgram(lucid, contracts, {
        ...applyConfig,
        attestation: {
          ...threshold,
          datum: {
            ...threshold.datum,
            availability_commitment: {
              ...threshold.datum.availability_commitment,
              tranche_descriptors: [
                { ...first!, chunk_commitment: "00".repeat(32) },
                ...rest,
              ],
            },
          },
        },
      }),
    );
    await assertAvailabilityRefusal(
      substituted.complete({ coinSelection: true, localUPLCEval: true }),
      { purpose: "mint", script: "da-attestation minting" },
      f.scriptNames,
    );
  }
  const apply = await Effect.runPromise(
    SDK.incompleteApplyDaAttestationToStateQueueTxProgram(
      lucid,
      contracts,
      applyConfig,
    ),
  );
  await f.submit("attestation apply against the pooled bond", apply, true);
  const [queue] = await lucid.utxosAtWithUnit(
    contracts.stateQueue.spendingScriptAddress,
    f.queueUnit,
  );
  if (!queue?.datum) throw new Error("Apply omitted the queue node");
  const commitmentHash = SDK.daAvailabilityCommitmentHash(f.commitment);
  const node = Data.castFrom(
    (await Effect.runPromise(SDK.getLinkedListNodeViewFromUTxO(queue))).data,
    SDK.StateQueueNode,
  );
  expect(node.da_attestation).toEqual({
    Attested: { commitment_hash: commitmentHash },
  });
  return { queue, commitment: f.commitment, commitmentHash };
};

const coordinate = (ctx: AvailabilityLayout, policy: string) =>
  Data.to(
    { Coordinate: { mint_redeemer_index: mintIndex(ctx, policy) } },
    SDK.DaAvailabilitySpendRedeemer,
  );
const queueUpdate = (
  ctx: AvailabilityLayout,
  policy: string,
  queue: UTxO,
  outputIndex: bigint,
) =>
  Data.to(
    {
      AvailabilityStatusUpdate: {
        state_queue_input_index: index(ctx, queue),
        state_queue_output_index: outputIndex,
        availability_mint_redeemer_index: mintIndex(ctx, policy),
      },
    },
    SDK.StateQueueSpendRedeemer,
  );
const yieldTx = (
  f: AvailabilityFixture,
  tx: TxBuilder,
  arm: keyof AvailabilityFixture["contracts"]["availabilityChallenge"]["yields"],
) =>
  tx.withdraw(
    SDK.scriptRewardAddress(
      "Preprod",
      f.contracts.availabilityChallenge.yields[arm].withdrawalScript,
    ),
    0n,
    Data.void(),
  );

/**
 * A hand-built mirror of `OpenChallenge`, so negatives can vary one field.
 * Inputs: the challenger's exact funding coin and the Attested queue node.
 * Outputs: the record (0), the node now `Challenged` (1), the tranche
 * threads (2..), the terminal accumulator (last). The commitment is explicit:
 * it must be the preimage of the node's `Attested{commitment_hash}`.
 */
export const openAvailability = async (
  f: AvailabilityFixture,
  attested: {
    readonly queue: UTxO;
    readonly commitment: SDK.DaAvailabilityCommitment;
  },
  validity: { validFrom?: bigint; validTo?: bigint } = {},
) => {
  const { lucid, contracts } = f;
  const parameters = f.parameters;
  lucid.selectWallet.fromPrivateKey(f.challenger.privateKey);
  const fee = parameters.max_open_fee_lovelace;
  const fundingLovelace =
    parameters.challenger_bond_lovelace +
    parameters.challenge_record_lovelace +
    fee;
  const fundingOutputs = await f.submit(
    "prepare isolated challenger funding",
    lucid.newTx().pay.ToAddress(f.challenger.address, {
      lovelace: fundingLovelace,
    }),
    true,
  );
  const funding = fundingOutputs.find(
    (utxo) => utxo.assets.lovelace === fundingLovelace,
  );
  if (!funding) throw new Error("Missing isolated challenger funding");
  const validFrom = validity.validFrom ?? BigInt(f.emulator.now());
  const validTo = validity.validTo ?? validFrom + 60_000n;
  const queue = attested.queue;
  const queueView = await Effect.runPromise(
    SDK.getLinkedListNodeViewFromUTxO(queue),
  );
  const queueNode = Data.castFrom(queueView.data, SDK.StateQueueNode);
  const planAt = (
    openedAt: bigint,
    commitment: SDK.DaAvailabilityCommitment = attested.commitment,
  ) =>
    SDK.buildDaAvailabilityChallengeDatumPlan({
      commitment,
      challengerFundingOutRef: outRef(funding),
      challenger: f.challengerKey,
      openedAt,
      parameters,
    });
  // The validator anchors the response window at the inclusive upper validity
  // bound; the ledger's upper end is exclusive.
  const plan = planAt(validTo - 1n);
  const policy = contracts.availabilityChallenge.policyId;
  const address = contracts.availabilityChallenge.spendingScriptAddress;
  const terminalUnit =
    policy +
    SDK.daAvailabilityTerminalAccumulatorAssetName(plan.challengeAssetName);
  const build = (
    options: {
      omitSigner?: boolean;
      omitYield?: boolean;
      wrongYield?: boolean;
      /** Datums anchored at this `opened_at` instead of the upper bound. */
      anchorAt?: bigint;
      /**
       * Records (and marks the node Challenged with) this commitment instead
       * of the preimage of the node's `Attested{commitment_hash}`.
       */
      commitment?: SDK.DaAvailabilityCommitment;
    } = {},
  ) => {
    const outputs = planAt(
      options.anchorAt ?? validTo - 1n,
      options.commitment ?? attested.commitment,
    );
    const challengedQueue = SDK.encodeLinkedListNodeView({
      ...queueView,
      data: SDK.castStateQueueNodeToData({
        ...queueNode,
        da_attestation: {
          Challenged: {
            commitment_hash: SDK.daAvailabilityCommitmentHash(
              outputs.record.commitment,
            ),
            challenge_asset_name: plan.challengeAssetName,
          },
        },
      }) as SDK.LinkedListNodeView["data"],
    });
    const yieldReference = f.reference(
      options.wrongYield
        ? "availability-challenge close withdrawal"
        : "availability-challenge open withdrawal",
    );
    const ctx: AvailabilityLayout = {
      inputs: [funding, queue],
      policies: [policy],
      references: [
        f.hubOracleRefInput,
        f.reference("availability-challenge minting"),
        yieldReference,
        f.reference("state-queue spending"),
      ],
    };
    const mint: Assets = {
      [policy + plan.challengeAssetName]: 1n,
      [terminalUnit]: 1n,
    };
    for (let i = 0; i < plan.trancheThreads.length; i += 1)
      mint[
        policy +
          SDK.daAvailabilityTrancheAssetName({
            challengeAssetName: plan.challengeAssetName,
            trancheIndex: i,
          })
      ] = 1n;
    let tx = lucid
      .newTx()
      .setMinFee(fee)
      .validFrom(Number(validFrom))
      .validTo(Number(validTo))
      .collectFrom([funding])
      .collectFrom([queue], queueUpdate(ctx, policy, queue, 1n))
      .readFrom([...ctx.references])
      .mintAssets(
        mint,
        Data.to(
          {
            OpenChallenge: {
              yield_to_ref_input_index: refIndex(ctx, yieldReference),
              hub_oracle_ref_input_index: refIndex(ctx, f.hubOracleRefInput),
              record_output_index: 0n,
              challenger_input_index: index(ctx, funding),
              state_queue_input_index: index(ctx, queue),
              state_queue_output_index: 1n,
              first_tranche_output_index: 2n,
              terminal_accumulator_output_index: BigInt(
                2 + plan.trancheThreads.length,
              ),
              challenger: f.challengerKey,
            },
          },
          SDK.DaAvailabilityMintRedeemer,
        ),
      )
      .pay.ToContract(
        address,
        inline(
          SDK.encodeDaAvailabilityChallengeRecord(outputs.record, parameters),
        ),
        {
          lovelace: outputs.recordLovelace,
          [policy + plan.challengeAssetName]: 1n,
        },
      )
      .pay.ToContract(queue.address, inline(challengedQueue), queue.assets);
    for (let i = 0; i < plan.trancheThreads.length; i += 1)
      tx = tx.pay.ToContract(
        address,
        inline(
          SDK.encodeDaAvailabilityTrancheDatum(outputs.trancheThreads[i]!),
        ),
        {
          lovelace: plan.trancheFunding[i]!.initialLovelace,
          [policy +
          SDK.daAvailabilityTrancheAssetName({
            challengeAssetName: plan.challengeAssetName,
            trancheIndex: i,
          })]: 1n,
        },
      );
    tx = tx.pay.ToContract(
      address,
      inline(
        SDK.encodeDaAvailabilityTerminalAccumulatorDatum(
          outputs.terminalAccumulator,
        ),
      ),
      { lovelace: plan.terminalAccumulatorFundingLovelace, [terminalUnit]: 1n },
    );
    if (!options.omitSigner) tx = tx.addSignerKey(f.challengerKey);
    if (!options.omitYield)
      tx = yieldTx(f, tx, options.wrongYield ? "close" : "open");
    return tx;
  };
  return {
    attested,
    funding,
    plan,
    policy,
    address,
    terminalUnit,
    build,
    async submit() {
      const outputs = await f.submit(
        `open ${plan.trancheThreads.length} tranches`,
        build(),
      );
      return {
        record: outputs[0]!,
        queue: outputs[1]!,
        threads: outputs.slice(2, 2 + plan.trancheThreads.length),
        terminal: outputs[2 + plan.trancheThreads.length]!,
      };
    },
  };
};

export const buildAvailabilityPublication = (
  f: AvailabilityFixture,
  thread: UTxO,
  publication: SDK.DaAvailabilityPublicationDatum,
  previousCarrier?: UTxO,
  options: { badChunk?: boolean } = {},
) => {
  const datum = Data.from(thread.datum!, SDK.DaAvailabilityTrancheDatum);
  const parameters = f.parameters;
  if (!("Active" in datum))
    throw new Error("Only an active tranche accepts a publication");
  // A publication may not stay valid past the response deadline, which the
  // selected profile's response window can place inside the default range.
  const validFrom = BigInt(f.emulator.now());
  // At or past the deadline the clamped range is empty or inverted, so fail
  // here with the cause instead of submitting a transaction that cannot land.
  if (validFrom >= datum.Active.response_deadline)
    throw new Error(
      `Publication built at or after the response deadline (now=${validFrom}, deadline=${datum.Active.response_deadline})`,
    );
  const deadlineUpper = datum.Active.response_deadline + 1n;
  const validTo =
    validFrom + 60_000n < deadlineUpper ? validFrom + 60_000n : deadlineUpper;
  const geometry = SDK.availabilityResponseGeometry({
    chunkByteLength: Number(parameters.response_geometry.chunk_byte_length),
    trancheByteLength: Number(parameters.response_geometry.tranche_byte_length),
    maxTrancheCount: Number(parameters.response_geometry.max_tranche_count),
  });
  const next = SDK.advanceDaAvailabilityTranche({
    active: datum,
    publication,
    responseGeometry: geometry,
    inclusiveValidityUpper: validTo - 1n,
    carrierOutputIndex: 1n,
  });
  const fee = parameters.max_publication_fee_lovelace;
  const carrierLovelace =
    calculateMinLovelaceFromUTxO(
      AVAILABILITY_EMULATOR_PARAMETERS.coinsPerUtxoByte,
      {
        txHash: "00".repeat(32),
        outputIndex: 1,
        address: thread.address,
        assets: { lovelace: 0n },
        datum: Data.to(publication, SDK.DaAvailabilityPublicationDatum),
      },
    ) + 100_000n;
  const nextLovelace =
    thread.assets.lovelace +
    (previousCarrier?.assets.lovelace ?? 0n) -
    carrierLovelace -
    fee;
  const mutated = options.badChunk
    ? { ...publication, chunk_hash: "00".repeat(32) }
    : publication;
  const ctx: AvailabilityLayout = {
    inputs: [thread, ...(previousCarrier ? [previousCarrier] : [])],
    references: [f.reference("availability-challenge spending")],
    policies: [],
  };
  let tx = f.lucid
    .newTx()
    .setMinFee(fee)
    .validFrom(Number(validFrom))
    .validTo(Number(validTo))
    .readFrom([f.reference("availability-challenge spending")])
    .collectFrom(
      [thread],
      Data.to(
        {
          AdvanceTranche: {
            thread_output_index: 0n,
            carrier_output_index: 1n,
            m_previous_carrier_input_index: previousCarrier
              ? index(ctx, previousCarrier)
              : null,
          },
        },
        SDK.DaAvailabilitySpendRedeemer,
      ),
    )
    .pay.ToContract(
      thread.address,
      inline(SDK.encodeDaAvailabilityTrancheDatum(next)),
      { ...thread.assets, lovelace: nextLovelace },
    )
    .pay.ToContract(
      thread.address,
      inline(Data.to(mutated, SDK.DaAvailabilityPublicationDatum)),
      { lovelace: carrierLovelace },
    );
  if (previousCarrier)
    tx = tx.collectFrom(
      [previousCarrier],
      Data.to(
        {
          ConsumeCarrier: {
            thread_input_index: index(ctx, thread),
            thread_spend_redeemer_index: position(spendingInputs(ctx), thread),
          },
        },
        SDK.DaAvailabilitySpendRedeemer,
      ),
    );
  return tx;
};

/** `SettleTranche`, reading the challenge record as a reference input. */
export const buildAvailabilitySettlement = (
  f: AvailabilityFixture,
  open: OpenAvailability,
  record: UTxO,
  terminal: UTxO,
  thread: UTxO,
  carrier?: UTxO,
  options: { validityLower?: bigint; bypassDeadlinePlanner?: boolean } = {},
) => {
  const terminalDatum = Data.from(
    terminal.datum!,
    SDK.DaAvailabilityTerminalAccumulatorDatum,
  );
  const threadDatum = Data.from(thread.datum!, SDK.DaAvailabilityTrancheDatum);
  const lower = options.validityLower ?? BigInt(f.emulator.now());
  const fee = f.parameters.max_settlement_fee_lovelace;
  const settlement = SDK.planDaAvailabilitySettlement({
    commitment: open.attested.commitment,
    terminalAccumulator: terminalDatum,
    tranche: threadDatum,
    threadLovelace: thread.assets.lovelace,
    carrierLovelace: carrier?.assets.lovelace ?? 0n,
    transactionFeeLovelace: fee,
    inclusiveValidityLower: options.bypassDeadlinePlanner
      ? open.plan.responseDeadline
      : lower,
    parameters: f.parameters,
  });
  const trancheIndex = Number(terminalDatum.next_tranche_index);
  const ctx: AvailabilityLayout = {
    inputs: [terminal, thread, ...(carrier ? [carrier] : [])],
    policies: [open.policy],
    references: [
      record,
      f.reference("availability-challenge spending"),
      f.reference("availability-challenge minting"),
      f.reference("availability-challenge settle withdrawal"),
    ],
  };
  let tx = f.lucid
    .newTx()
    .setMinFee(fee)
    .validFrom(Number(lower))
    .validTo(Number(lower + 60_000n))
    .collectFrom([...ctx.inputs], coordinate(ctx, open.policy))
    .readFrom([...ctx.references])
    .mintAssets(
      {
        [open.policy +
        SDK.daAvailabilityTrancheAssetName({
          challengeAssetName: open.plan.challengeAssetName,
          trancheIndex,
        })]: -1n,
      },
      Data.to(
        {
          SettleTranche: {
            yield_to_ref_input_index: refIndex(
              ctx,
              f.reference("availability-challenge settle withdrawal"),
            ),
            record_ref_input_index: refIndex(ctx, record),
            terminal_accumulator_input_index: index(ctx, terminal),
            terminal_accumulator_output_index: 0n,
            tranche_input_index: index(ctx, thread),
            carrier_input_index: carrier ? index(ctx, carrier) : null,
          },
        },
        SDK.DaAvailabilityMintRedeemer,
      ),
    )
    .pay.ToContract(
      open.address,
      inline(
        SDK.encodeDaAvailabilityTerminalAccumulatorDatum(
          settlement.nextTerminalAccumulator,
        ),
      ),
      { lovelace: settlement.nextTerminalLovelace, [open.terminalUnit]: 1n },
    );
  tx = yieldTx(f, tx, "settle");
  return tx;
};

/**
 * `CloseChallenge`: burns the record and terminal, marks the node Published
 * (0) and refunds the challenger `remaining - fee + challenge_record` (1).
 * The pooled bond is not touched.
 */
export const buildAvailabilityClose = (
  f: AvailabilityFixture,
  open: OpenAvailability,
  record: UTxO,
  queue: UTxO,
  terminal: UTxO,
  options: { redirectRefund?: boolean } = {},
) => {
  const fee = f.parameters.max_close_fee_lovelace;
  const ctx: AvailabilityLayout = {
    inputs: [record, terminal, queue],
    policies: [open.policy],
    references: [
      f.hubOracleRefInput,
      f.reference("state-queue spending"),
      f.reference("availability-challenge spending"),
      f.reference("availability-challenge minting"),
      f.reference("availability-challenge close withdrawal"),
    ],
  };
  const queueView = SDK.getLinkedListNodeViewFromUTxO(queue);
  const view = Effect.runSync(queueView);
  const node = Data.castFrom(view.data, SDK.StateQueueNode);
  const queueDatum = SDK.encodeLinkedListNodeView({
    ...view,
    data: SDK.castStateQueueNodeToData({
      ...node,
      da_attestation: {
        Published: {
          terminal_commitment: SDK.daAvailabilityPublishedTerminalCommitment(
            open.attested.commitment,
          ),
        },
      },
    }) as SDK.LinkedListNodeView["data"],
  });
  let tx = f.lucid
    .newTx()
    .setMinFee(fee)
    .collectFrom([record, terminal], coordinate(ctx, open.policy))
    .collectFrom([queue], queueUpdate(ctx, open.policy, queue, 0n))
    .readFrom([...ctx.references])
    .mintAssets(
      {
        [open.policy + open.plan.challengeAssetName]: -1n,
        [open.terminalUnit]: -1n,
      },
      Data.to(
        {
          CloseChallenge: {
            yield_to_ref_input_index: refIndex(
              ctx,
              f.reference("availability-challenge close withdrawal"),
            ),
            hub_oracle_ref_input_index: refIndex(ctx, f.hubOracleRefInput),
            record_input_index: index(ctx, record),
            terminal_accumulator_input_index: index(ctx, terminal),
            state_queue_input_index: index(ctx, queue),
            state_queue_output_index: 0n,
            challenger_refund_output_index: 1n,
          },
        },
        SDK.DaAvailabilityMintRedeemer,
      ),
    )
    .pay.ToContract(queue.address, inline(queueDatum), queue.assets)
    .pay.ToAddress(
      options.redirectRefund ? f.responder.address : f.challenger.address,
      {
        lovelace:
          terminal.assets.lovelace -
          fee +
          f.parameters.challenge_record_lovelace,
      },
    );
  tx = yieldTx(f, tx, "close");
  return tx;
};

/**
 * `TimeoutChallenge` with the head removal and the pool's `Slash` in one
 * transaction (hand-built mirror of the SDK builder). Outputs: the continued
 * root (0), the Idle correction lock (1), the ONE challenger output
 * `remaining - c + challenge_record + payout` (2), the pool with
 * `pool - taken` beside its NFT and its datum unchanged (3), the removed
 * node's rent (4). The fee is exactly `feePart + c`, `feePart =
 * min(penalty, taken)`.
 */
export const buildAvailabilityTimeout = async (
  f: AvailabilityFixture,
  open: OpenAvailability,
  record: UTxO,
  queue: UTxO,
  terminal: UTxO,
  options: {
    early?: boolean;
    /** Pays the challenger output to the responder instead. */
    redirectPayout?: boolean;
    /** The challenger's fee contribution `c` (default 0). */
    challengerFeeLovelace?: bigint;
    pool?: UTxO;
  } = {},
) => {
  const pool = options.pool ?? (await f.getPool());
  const slash = SDK.planDaBondPoolSlash({
    poolLovelace: pool.assets.lovelace,
    parameters: f.parameters,
  });
  const terminalDatum = Data.from(
    terminal.datum!,
    SDK.DaAvailabilityTerminalAccumulatorDatum,
  );
  const plan = SDK.planDaAvailabilityTimeout({
    poolLovelace: pool.assets.lovelace,
    remainingChallengerLovelace: terminalDatum.remaining_challenger_lovelace,
    challengerFeeLovelace: options.challengerFeeLovelace ?? 0n,
    parameters: f.parameters,
  });
  expect(plan.feePart).toBe(slash.feePart);
  // Exact: the inputs pay the outputs and `feePart + c`, completed without
  // coin selection by `f.submit`, so no change output exists.
  const fee = plan.feeLovelace;
  const lower = options.early
    ? open.plan.responseDeadline - 1_000n
    : BigInt(f.emulator.now());
  const queuePolicy = f.contracts.stateQueue.policyId;
  const ctx: AvailabilityLayout = {
    inputs: [record, terminal, queue, f.rootUtxo, f.correctionLockUtxo, pool],
    policies: [open.policy, queuePolicy],
    references: [
      f.hubOracleRefInput,
      f.reference("state-queue spending"),
      f.reference("state-queue minting"),
      f.reference("state-queue unavailable-timeout withdrawal"),
      f.reference("correction-lock spending"),
      f.reference("availability-challenge spending"),
      f.reference("availability-challenge minting"),
      f.reference("availability-challenge timeout withdrawal"),
      f.reference("da-bond-pool spending"),
    ],
  };
  const rootDatum = SDK.encodeLinkedListNodeView({
    ...f.rootDatum,
    next: "Empty",
  });
  let tx = f.lucid
    .newTx()
    .setMinFee(fee)
    .validFrom(Number(lower))
    .validTo(Number(lower + 60_000n))
    .collectFrom([record, terminal], coordinate(ctx, open.policy))
    .collectFrom(
      [queue, f.rootUtxo],
      Data.to("LinkedListMutation", SDK.StateQueueSpendRedeemer),
    )
    .collectFrom(
      [f.correctionLockUtxo],
      Data.to(
        {
          Correct: {
            hub_oracle_ref_input_index: refIndex(ctx, f.hubOracleRefInput),
          },
        },
        SDK.CorrectionLockRedeemer,
      ),
    )
    .collectFrom(
      [pool],
      Data.to(
        {
          Slash: {
            hub_oracle_ref_input_index: refIndex(ctx, f.hubOracleRefInput),
            state_queue_mint_redeemer_index: mintIndex(ctx, queuePolicy),
            correction_lock_input_index: index(ctx, f.correctionLockUtxo),
            output_index: 3n,
          },
        },
        SDK.DaBondPoolSpendRedeemer,
      ),
    )
    .readFrom([...ctx.references])
    .mintAssets(
      {
        [open.policy + open.plan.challengeAssetName]: -1n,
        [open.terminalUnit]: -1n,
      },
      Data.to(
        {
          TimeoutChallenge: {
            yield_to_ref_input_index: refIndex(
              ctx,
              f.reference("availability-challenge timeout withdrawal"),
            ),
            hub_oracle_ref_input_index: refIndex(ctx, f.hubOracleRefInput),
            record_input_index: index(ctx, record),
            terminal_accumulator_input_index: index(ctx, terminal),
            state_queue_mint_redeemer_index: mintIndex(ctx, queuePolicy),
            pool_input_index: index(ctx, pool),
            pool_output_index: 3n,
            challenger_refund_output_index: 2n,
          },
        },
        SDK.DaAvailabilityMintRedeemer,
      ),
    )
    .mintAssets(
      { [f.queueUnit]: -1n },
      Data.to(
        {
          RemoveUnavailableBlockAfterTimeout: {
            yield_to_ref_input_index: refIndex(
              ctx,
              f.reference("state-queue unavailable-timeout withdrawal"),
            ),
            unavailable_header_hash: f.target.headerHash,
            challenge_asset_name: open.plan.challengeAssetName,
            removal_approach: {
              RemoveTimedOutHead: {
                confirmed_state_input_outref: outRef(f.rootUtxo),
                confirmed_state_output_index: 0n,
              },
            },
          },
        },
        SDK.StateQueueRedeemer,
      ),
    )
    .pay.ToContract(f.rootUtxo.address, inline(rootDatum), f.rootUtxo.assets)
    .pay.ToContract(
      f.correctionLockUtxo.address,
      inline(Data.to("Idle", SDK.CorrectionLockDatum)),
      f.correctionLockUtxo.assets,
    )
    .pay.ToAddress(
      options.redirectPayout ? f.responder.address : f.challenger.address,
      { lovelace: plan.challengerOutputLovelace },
    )
    .pay.ToContract(pool.address, inline(pool.datum!), {
      lovelace: plan.poolOutputLovelace,
      [f.poolUnit]: 1n,
    })
    .pay.ToAddress(f.responder.address, { lovelace: queue.assets.lovelace })
    .withdraw(
      SDK.scriptRewardAddress(
        "Preprod",
        f.contracts.stateQueue.yields.unavailableTimeout.withdrawalScript,
      ),
      0n,
      Data.void(),
    );
  tx = yieldTx(f, tx, "timeout");
  return { tx, plan, pool };
};

export const advanceAvailabilityDeadline = (
  f: AvailabilityFixture,
  open: OpenAvailability,
) => {
  const slots = Math.max(
    1,
    Math.ceil((Number(open.plan.responseDeadline) - f.emulator.now()) / 1_000) +
      1,
  );
  f.emulator.awaitSlot(slots);
};

export { credentialToRewardAddress };
