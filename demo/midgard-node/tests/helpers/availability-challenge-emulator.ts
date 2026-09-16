import * as SDK from "@al-ft/midgard-sdk";
import {
  type Assets,
  calculateMinLovelaceFromUTxO,
  CML,
  Data,
  Emulator,
  type EmulatorAccount,
  generateEmulatorAccountFromPrivateKey,
  paymentCredentialOf,
  type TxBuilder,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { Effect } from "effect";
import { expect } from "vitest";

import {
  nodeRuntimeReferenceScriptTargets,
  referenceScriptTargetsByCommand,
} from "../../src/transactions/reference-scripts.js";
import { TEST_AVAILABILITY_PARAMETERS } from "./availability-challenge.js";
import {
  createMainnetEmulatorLucid,
  MAINNET_PROTOCOL_PARAMETERS,
} from "./mainnet-protocol-parameters.js";
import { loadRealMidgardContractsForTest } from "./real-midgard-contracts.js";

export const AVAILABILITY_EMULATOR_PARAMETERS = {
  ...MAINNET_PROTOCOL_PARAMETERS,
} as const;

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

const measure = (
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
const h32 = (byte: string) => byte.repeat(32);
const outRef = SDK.outputReferenceFromUTxO;

export type AvailabilityFixture = Awaited<
  ReturnType<typeof createAvailabilityFixture>
>;
export type OpenAvailability = Awaited<ReturnType<typeof openAvailability>>;

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

/**
 * The genesis fixture represents an already deployed protocol and committed block.
 * No attestation, bond, challenge, tranche, carrier or terminal asset is seeded:
 * every availability state below is produced by an evaluated ledger transaction.
 */
export const createAvailabilityFixture = async (
  payloadBytes = 14_021,
  descendantCount = 0,
) => {
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
  const preliminaryLucid = await createMainnetEmulatorLucid(preliminary);
  preliminaryLucid.selectWallet.fromPrivateKey(publisher.privateKey);
  const authPolicy = await SDK.createReferenceScriptAuthPolicy(
    preliminaryLucid,
    preliminary.now(),
  );
  const contracts = await loadRealMidgardContractsForTest(
    { txHash: "00".repeat(32), outputIndex: 0 },
    authPolicy,
  );
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
    header: {
      prevUtxosRoot: h32("01"),
      utxosRoot: h32("02"),
      withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      ...SDK.EMPTY_HEADER_TRANSITION_COMMITMENTS,
      startTime: BigInt(now - 1_000),
      endTime: BigInt(now),
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
  const genesis = (
    address: string,
    assets: Assets,
    datum: string,
  ): EmulatorAccount => ({
    seedPhrase: "",
    privateKey: "",
    address,
    assets,
    outputData: { inline: datum },
  });
  const emulator = new Emulator(
    [
      responder,
      challenger,
      publisher,
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
        { lovelace: 4_000_000n, [queueUnit]: 1n },
        SDK.encodeLinkedListNodeView(queueDatum),
      ),
      genesis(
        contracts.stateQueue.spendingScriptAddress,
        { lovelace: 4_000_000n, [rootUnit]: 1n },
        SDK.encodeLinkedListNodeView(rootDatum),
      ),
      ...descendants.map(({ hash, datum }) =>
        genesis(
          contracts.stateQueue.spendingScriptAddress,
          {
            lovelace: 4_000_000n,
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
    ],
    AVAILABILITY_EMULATOR_PARAMETERS,
  );
  const lucid = await createMainnetEmulatorLucid(emulator);
  const publishingLucid = await createMainnetEmulatorLucid(emulator);
  lucid.selectWallet.fromPrivateKey(responder.privateKey);
  publishingLucid.selectWallet.fromPrivateKey(publisher.privateKey);
  const measurements: AvailabilityMeasurement[] = [];
  const submit = async (
    name: string,
    builder: TxBuilder,
    coinSelection = false,
  ) => {
    const unsigned = await builder
      .complete({ localUPLCEval: true, coinSelection })
      .catch((cause: unknown) => {
        throw new Error(`Availability stage ${name}: ${String(cause)}`, {
          cause,
        });
      });
    const signed = await unsigned.sign.withWallet().complete();
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
    measurements.push(measure(name, signed.toCBOR(), refs));
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
  const targets = [
    ...nodeRuntimeReferenceScriptTargets(contracts),
    ...referenceScriptTargetsByCommand(contracts).da,
  ].filter(
    ({ name }) =>
      name.startsWith("availability-challenge ") ||
      name.startsWith("da-attestation ") ||
      [
        "state-queue minting",
        "state-queue spending",
        "state-queue unavailable-timeout withdrawal",
        "correction-lock spending",
      ].includes(name),
  );
  const references = new Map<string, UTxO>();
  for (const target of targets) {
    if (references.has(target.name)) continue;
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
    const measurement = measure(`publish ${target.name}`, signed.toCBOR());
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
  if (
    !hubOracleRefInput ||
    !daParamsUtxo ||
    !queueUtxo ||
    !rootUtxo ||
    !correctionLockUtxo
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
    bondOwner: responderKey,
    responseGeometry: SDK.availabilityResponseGeometry({
      chunkByteLength: Number(
        TEST_AVAILABILITY_PARAMETERS.response_geometry.chunk_byte_length,
      ),
      trancheByteLength: Number(
        TEST_AVAILABILITY_PARAMETERS.response_geometry.tranche_byte_length,
      ),
      maxTrancheCount: Number(
        TEST_AVAILABILITY_PARAMETERS.response_geometry.max_tranche_count,
      ),
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
    availabilityChallengeMinting: reference("availability-challenge minting"),
    availabilityChallengeBondWithdrawal: reference(
      "availability-challenge bond withdrawal",
    ),
  };
  return {
    emulator,
    lucid,
    contracts,
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
    target,
    payload,
    commitment,
    daReferences,
    reference,
    measurements,
    submit,
  };
};

export const attestAvailability = async (
  f: AvailabilityFixture,
  options: { refuseBondSubstitution?: boolean } = {},
) => {
  const { lucid, contracts } = f;
  lucid.selectWallet.fromPrivateKey(f.responder.privateKey);
  const init = await Effect.runPromise(
    SDK.incompleteInitDaAttestationTxProgram(lucid, contracts, {
      daParamsUtxo: f.daParamsUtxo,
      daParamsDatum: f.daParamsDatum,
      target: f.target,
      referenceScripts: f.daReferences,
      attestationOutputLovelace: TEST_AVAILABILITY_PARAMETERS.da_bond_lovelace,
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
    hubOracleRefInput: f.hubOracleRefInput,
    validityRange: {
      validFrom: BigInt(f.emulator.now()),
      validTo: BigInt(f.emulator.now() + 60_000),
    },
  };
  if (options.refuseBondSubstitution) {
    // The consumed attestation carries the real committee-signed commitment;
    // a substituted off-chain bond owner must be refused by the bond yield.
    const substituted = await Effect.runPromise(
      SDK.incompleteApplyDaAttestationToStateQueueTxProgram(lucid, contracts, {
        ...applyConfig,
        attestation: {
          ...threshold,
          datum: {
            ...threshold.datum,
            availability_commitment: {
              ...threshold.datum.availability_commitment,
              bond_owner: f.challengerKey,
            },
          },
        },
      }),
    );
    await assertAvailabilityRefusal(substituted, true);
  }
  const apply = await Effect.runPromise(
    SDK.incompleteApplyDaAttestationToStateQueueTxProgram(
      lucid,
      contracts,
      applyConfig,
    ),
  );
  await f.submit("attestation apply and retained bond mint", apply, true);
  const bondAssetName = SDK.daAvailabilityBondAssetName(outRef(threshold.utxo));
  const [bond] = await lucid.utxosAtWithUnit(
    contracts.availabilityChallenge.spendingScriptAddress,
    contracts.availabilityChallenge.policyId + bondAssetName,
  );
  const [queue] = await lucid.utxosAtWithUnit(
    contracts.stateQueue.spendingScriptAddress,
    f.queueUnit,
  );
  if (!bond?.datum || !queue?.datum)
    throw new Error("Apply omitted bond or queue");
  expect(Data.from(bond.datum, SDK.DaAvailabilityBondDatum)).toMatchObject({
    Available: { da_bond_asset_name: bondAssetName },
  });
  return { bond, queue, bondAssetName };
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

export const openAvailability = async (
  f: AvailabilityFixture,
  bonded: Awaited<ReturnType<typeof attestAvailability>>,
) => {
  const { lucid, contracts } = f;
  lucid.selectWallet.fromPrivateKey(f.challenger.privateKey);
  const fee = TEST_AVAILABILITY_PARAMETERS.max_open_fee_lovelace;
  const fundingOutputs = await f.submit(
    "prepare isolated challenger bond and collateral",
    lucid
      .newTx()
      .pay.ToAddress(f.challenger.address, {
        lovelace: TEST_AVAILABILITY_PARAMETERS.challenger_bond_lovelace + fee,
      })
      .pay.ToAddress(f.challenger.address, { lovelace: 10_000_000n }),
    true,
  );
  const funding = fundingOutputs.find(
    (utxo) =>
      utxo.assets.lovelace ===
      TEST_AVAILABILITY_PARAMETERS.challenger_bond_lovelace + fee,
  );
  if (!funding) throw new Error("Missing isolated challenger funding");
  const openedAt = BigInt(f.emulator.now());
  const available = Data.from(bonded.bond.datum!, SDK.DaAvailabilityBondDatum);
  const plan = SDK.buildDaAvailabilityChallengeDatumPlan({
    availableBond: available,
    bondInputOutRef: outRef(bonded.bond),
    challenger: f.challengerKey,
    openedAt,
    parameters: TEST_AVAILABILITY_PARAMETERS,
  });
  const policy = contracts.availabilityChallenge.policyId;
  const address = contracts.availabilityChallenge.spendingScriptAddress;
  const terminalUnit =
    policy +
    SDK.daAvailabilityTerminalAccumulatorAssetName(plan.challengeAssetName);
  const challengedQueue = SDK.encodeLinkedListNodeView({
    ...f.target.stateQueueUtxo.datum,
    data: SDK.castStateQueueNodeToData({
      ...f.target.stateQueueNode,
      da_attestation: {
        Challenged: {
          da_bond_asset_name: bonded.bondAssetName,
          challenge_asset_name: plan.challengeAssetName,
        },
      },
    }) as SDK.LinkedListNodeView["data"],
  });
  const build = (
    options: {
      omitSigner?: boolean;
      omitYield?: boolean;
      wrongYield?: boolean;
    } = {},
  ) => {
    const yieldReference = f.reference(
      options.wrongYield
        ? "availability-challenge close withdrawal"
        : "availability-challenge open withdrawal",
    );
    const ctx: AvailabilityLayout = {
      inputs: [bonded.bond, funding, bonded.queue],
      policies: [policy],
      references: [
        f.hubOracleRefInput,
        f.reference("availability-challenge minting"),
        f.reference("availability-challenge spending"),
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
      .validFrom(Number(openedAt))
      .validTo(Number(openedAt + 60_000n))
      .collectFrom([bonded.bond], coordinate(ctx, policy))
      .collectFrom([funding])
      .collectFrom([bonded.queue], queueUpdate(ctx, policy, bonded.queue, 1n))
      .readFrom([...ctx.references])
      .mintAssets(
        mint,
        Data.to(
          {
            OpenChallenge: {
              yield_to_ref_input_index: refIndex(ctx, yieldReference),
              hub_oracle_ref_input_index: refIndex(ctx, f.hubOracleRefInput),
              bond_input_index: index(ctx, bonded.bond),
              bond_output_index: 0n,
              challenger_input_index: index(ctx, funding),
              state_queue_input_index: index(ctx, bonded.queue),
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
        inline(SDK.encodeDaAvailabilityBondDatum(plan.challengedBond)),
        { ...bonded.bond.assets, [policy + plan.challengeAssetName]: 1n },
      )
      .pay.ToContract(
        bonded.queue.address,
        inline(challengedQueue),
        bonded.queue.assets,
      );
    for (let i = 0; i < plan.trancheThreads.length; i += 1)
      tx = tx.pay.ToContract(
        address,
        inline(SDK.encodeDaAvailabilityTrancheDatum(plan.trancheThreads[i]!)),
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
          plan.terminalAccumulator,
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
    bonded,
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
        bond: outputs[0]!,
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
  const parameters = TEST_AVAILABILITY_PARAMETERS;
  const geometry = SDK.availabilityResponseGeometry({
    chunkByteLength: Number(parameters.response_geometry.chunk_byte_length),
    trancheByteLength: Number(parameters.response_geometry.tranche_byte_length),
    maxTrancheCount: Number(parameters.response_geometry.max_tranche_count),
  });
  const next = SDK.advanceDaAvailabilityTranche({
    active: datum,
    publication,
    responseGeometry: geometry,
    inclusiveValidityUpper: BigInt(f.emulator.now() + 60_000),
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
    .validFrom(f.emulator.now())
    .validTo(f.emulator.now() + 60_000)
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

export const buildAvailabilitySettlement = (
  f: AvailabilityFixture,
  open: OpenAvailability,
  bond: UTxO,
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
  const fee = TEST_AVAILABILITY_PARAMETERS.max_settlement_fee_lovelace;
  const settlement = SDK.planDaAvailabilitySettlement({
    commitment: f.commitment,
    terminalAccumulator: terminalDatum,
    tranche: threadDatum,
    threadLovelace: thread.assets.lovelace,
    carrierLovelace: carrier?.assets.lovelace ?? 0n,
    transactionFeeLovelace: fee,
    inclusiveValidityLower: options.bypassDeadlinePlanner
      ? open.plan.responseDeadline
      : lower,
    parameters: TEST_AVAILABILITY_PARAMETERS,
  });
  const trancheIndex = Number(terminalDatum.next_tranche_index);
  const ctx: AvailabilityLayout = {
    inputs: [terminal, thread, ...(carrier ? [carrier] : [])],
    policies: [open.policy],
    references: [
      bond,
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
            bond_ref_input_index: refIndex(ctx, bond),
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

export const buildAvailabilityClose = (
  f: AvailabilityFixture,
  open: OpenAvailability,
  bond: UTxO,
  queue: UTxO,
  terminal: UTxO,
  options: { redirectRefund?: boolean } = {},
) => {
  const fee = TEST_AVAILABILITY_PARAMETERS.max_close_fee_lovelace;
  const ctx: AvailabilityLayout = {
    inputs: [bond, terminal, queue],
    policies: [open.policy],
    references: [
      f.hubOracleRefInput,
      f.reference("state-queue spending"),
      f.reference("availability-challenge spending"),
      f.reference("availability-challenge minting"),
      f.reference("availability-challenge close withdrawal"),
    ],
  };
  const queueDatum = SDK.encodeLinkedListNodeView({
    ...f.target.stateQueueUtxo.datum,
    data: SDK.castStateQueueNodeToData({
      ...f.target.stateQueueNode,
      da_attestation: {
        Published: {
          terminal_commitment: SDK.daAvailabilityPublishedTerminalCommitment(
            f.commitment,
          ),
        },
      },
    }) as SDK.LinkedListNodeView["data"],
  });
  let tx = f.lucid
    .newTx()
    .setMinFee(fee)
    .collectFrom([bond, terminal], coordinate(ctx, open.policy))
    .collectFrom([queue], queueUpdate(ctx, open.policy, queue, 0n))
    .readFrom([...ctx.references])
    .mintAssets(
      {
        [open.policy + open.bonded.bondAssetName]: -1n,
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
            bond_input_index: index(ctx, bond),
            terminal_accumulator_input_index: index(ctx, terminal),
            state_queue_input_index: index(ctx, queue),
            state_queue_output_index: 0n,
            da_refund_output_index: 1n,
            challenger_refund_output_index: 2n,
          },
        },
        SDK.DaAvailabilityMintRedeemer,
      ),
    )
    .pay.ToContract(queue.address, inline(queueDatum), queue.assets)
    .pay.ToAddress(
      options.redirectRefund ? f.challenger.address : f.responder.address,
      { lovelace: TEST_AVAILABILITY_PARAMETERS.da_bond_lovelace },
    )
    .pay.ToAddress(f.challenger.address, {
      lovelace: terminal.assets.lovelace - fee,
    });
  tx = yieldTx(f, tx, "close");
  return tx;
};

export const buildAvailabilityTimeout = (
  f: AvailabilityFixture,
  open: OpenAvailability,
  bond: UTxO,
  queue: UTxO,
  terminal: UTxO,
  options: { early?: boolean; redirectSlash?: boolean } = {},
) => {
  if (process.env.MIDGARD_PRINT_PROOF_FIT === "1")
    console.info(
      "timeout withdrawal order",
      [
        {
          name: "availability",
          hash: f.contracts.availabilityChallenge.yields.timeout
            .withdrawalScriptHash,
        },
        {
          name: "queue",
          hash: f.contracts.stateQueue.yields.unavailableTimeout
            .withdrawalScriptHash,
        },
      ].sort((a, b) => a.hash.localeCompare(b.hash)),
    );
  const fee = TEST_AVAILABILITY_PARAMETERS.max_timeout_fee_lovelace;
  const lower = options.early
    ? open.plan.responseDeadline - 1_000n
    : BigInt(f.emulator.now());
  const queuePolicy = f.contracts.stateQueue.policyId;
  const ctx: AvailabilityLayout = {
    inputs: [bond, terminal, queue, f.rootUtxo, f.correctionLockUtxo],
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
    .collectFrom([bond, terminal], coordinate(ctx, open.policy))
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
    .readFrom([...ctx.references])
    .mintAssets(
      {
        [open.policy + open.bonded.bondAssetName]: -1n,
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
            bond_input_index: index(ctx, bond),
            terminal_accumulator_input_index: index(ctx, terminal),
            state_queue_mint_redeemer_index: mintIndex(ctx, queuePolicy),
            da_slash_output_index: 2n,
            challenger_refund_output_index: 3n,
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
      options.redirectSlash ? f.responder.address : f.challenger.address,
      { lovelace: TEST_AVAILABILITY_PARAMETERS.da_bond_lovelace },
    )
    .pay.ToAddress(f.challenger.address, {
      lovelace: terminal.assets.lovelace - fee,
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
  return tx;
};

export const assertAvailabilityRefusal = async (
  builder: TxBuilder,
  coinSelection = false,
) => {
  await expect(
    builder.complete({ coinSelection, localUPLCEval: true }),
  ).rejects.toThrow(
    /(?:Error evaluated at |Builtin error: )[^\n]*spent budget:/,
  );
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
import { writeFileSync } from "node:fs";
import { join } from "node:path";
