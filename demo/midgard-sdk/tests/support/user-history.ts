/**
 * A deployed pair of event-history lists on the Lucid emulator, as a user's
 * deposit or withdrawal meets it: genuine applied list scripts sharing one
 * initialization nonce, a hub oracle under a native issuer, and the two list
 * minting scripts published as reference scripts, and the genuine tx-order
 * contracts the hub oracle names. The user's wallet is
 * separate from the deployer's, so its UTxOs are exactly the ones the test
 * gives it.
 */
import { readFileSync } from "node:fs";

import { PLUTUS_V3_CANONICAL_COST_MODEL_VIEW } from "@al-ft/midgard-core";
import {
  addressDataFromBech32,
  appendEventHistoryInitialization,
  buildEventHistoryDeployments,
  buildTxOrderValidators,
  type EventHistoryContractPair,
  type EventHistoryPayloadBounds,
  type EventHistoryRecipe,
  type FaultProofBlueprint,
  HubOracleDatum,
  parseFaultProofBlueprint,
  scriptRewardAddress,
  type UserHistoryContracts,
  type UserL1View,
} from "@al-ft/midgard-sdk";
import {
  Data,
  Emulator,
  type EmulatorAccount,
  fromText,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  type LucidEvolution,
  mintingPolicyToId,
  PROTOCOL_PARAMETERS_DEFAULT,
  type Script,
  scriptFromNative,
  type TxSignBuilder,
  type UTxO,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

/** Mainnet-shaped limits: protocol version 11 builtins costed, Van Rossem
 * transaction size and execution budgets. */
export const USER_HISTORY_PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  protocolMajorVersion: 11,
  costModels: {
    ...PROTOCOL_PARAMETERS_DEFAULT.costModels,
    PlutusV3: [
      ...PROTOCOL_PARAMETERS_DEFAULT.costModels.PlutusV3,
      ...PLUTUS_V3_CANONICAL_COST_MODEL_VIEW.slice(
        PROTOCOL_PARAMETERS_DEFAULT.costModels.PlutusV3.length,
      ),
    ],
  },
  maxTxSize: 16_384,
  maxTxExMem: 16_500_000n,
  maxTxExSteps: 10_000_000_000n,
  maxCollateralInputs: 3,
};

/** The repository's compiled blueprint (or MIDGARD_REAL_BLUEPRINT_PATH). */
export const loadUserHistoryBlueprint = (): FaultProofBlueprint =>
  parseFaultProofBlueprint(
    JSON.parse(
      readFileSync(
        process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
          new URL("../../../../onchain/aiken/plutus.json", import.meta.url),
        "utf8",
      ),
    ),
  );

export type UserHistoryScene = Awaited<
  ReturnType<typeof setupUserHistoryScene>
>;

export const setupUserHistoryScene = async ({
  blueprint,
  protectionDurationMs = 120_000n,
  bounds = {
    inlineLimitBytes: 512n,
    maxPayloadBytes: 5000n,
    maxPayloadNodes: 512n,
  },
  userCoins = [300_000_000n, 50_000_000n, 50_000_000n],
}: {
  readonly blueprint: FaultProofBlueprint;
  readonly protectionDurationMs?: bigint;
  readonly bounds?: EventHistoryPayloadBounds;
  /** The user's wallet: one UTxO per amount. */
  readonly userCoins?: readonly bigint[];
}) => {
  const [first, ...rest] = userCoins;
  if (first === undefined) throw new Error("The user needs at least one coin");
  const deployer = generateEmulatorAccount({ lovelace: 2_000_000_000n });
  const user: EmulatorAccount = generateEmulatorAccount({ lovelace: first });
  const emulator = new Emulator(
    [deployer, user],
    USER_HISTORY_PROTOCOL_PARAMETERS,
  );
  // Every Lucid on this emulator maps time to slots from the emulator's
  // genesis, so a validity bound that trails the clock stays after slot 0.
  const slotConfig = {
    zeroTime: emulator.now() - emulator.slot * 1000,
    zeroSlot: 0,
    slotLength: 1000,
  };
  const lucidFor = async (seedPhrase: string): Promise<LucidEvolution> => {
    const lucid = await Lucid(emulator, "Custom", { slotConfig });
    lucid.selectWallet.fromSeed(seedPhrase);
    return lucid;
  };
  const operator = await lucidFor(deployer.seedPhrase);
  const submit = async (tx: TxSignBuilder) => {
    const txHash = await (await tx.sign.withWallet().complete()).submit();
    emulator.awaitBlock();
    return txHash;
  };
  let split = operator
    .newTx()
    .pay.ToAddress(deployer.address, { lovelace: 5_000_000n });
  for (const lovelace of rest)
    split = split.pay.ToAddress(user.address, { lovelace });
  const splitHash = await submit(await split.complete());
  const [nonce] = await operator.utxosByOutRef([
    { txHash: splitHash, outputIndex: 0 },
  ]);
  if (nonce === undefined) throw new Error("Initialization nonce is missing");
  const funding = async () =>
    (await operator.wallet().getUtxos()).filter(
      (utxo) =>
        utxo.txHash !== nonce.txHash || utxo.outputIndex !== nonce.outputIndex,
    );

  const issuer = scriptFromNative({
    type: "sig",
    keyHash: getAddressDetails(deployer.address).paymentCredential!.hash,
  });
  const hubPolicy = mintingPolicyToId(issuer);
  const hubAddress = validatorToAddress("Custom", issuer);
  const pair: EventHistoryContractPair = buildEventHistoryDeployments({
    blueprint,
    network: "Custom",
    hubOraclePolicyId: hubPolicy,
    initializationNonce: nonce,
    protectionDurationMs,
    bounds,
  });
  const hubAddressData = Effect.runSync(addressDataFromBech32(hubAddress));
  const { txOrder } = buildTxOrderValidators({
    blueprint,
    network: "Custom",
    hubOraclePolicyId: hubPolicy,
  });
  const hubDatum: HubOracleDatum = {
    registered_operators: hubPolicy,
    active_operators: hubPolicy,
    retired_operators: hubPolicy,
    scheduler: hubPolicy,
    state_queue: hubPolicy,
    fraud_proof_catalogue: hubPolicy,
    fraud_proof: hubPolicy,
    deposit: pair.deposit.list.policyId,
    withdrawal: pair.withdrawal.list.policyId,
    tx_order: txOrder.policyId,
    settlement: hubPolicy,
    payout: hubPolicy,
    registered_operators_addr: hubAddressData,
    active_operators_addr: hubAddressData,
    retired_operators_addr: hubAddressData,
    scheduler_addr: hubAddressData,
    state_queue_addr: hubAddressData,
    fraud_proof_catalogue_addr: hubAddressData,
    fraud_proof_addr: hubAddressData,
    deposit_addr: Effect.runSync(
      addressDataFromBech32(pair.deposit.list.spendingScriptAddress),
    ),
    withdrawal_addr: Effect.runSync(
      addressDataFromBech32(pair.withdrawal.list.spendingScriptAddress),
    ),
    tx_order_addr: Effect.runSync(
      addressDataFromBech32(txOrder.spendingScriptAddress),
    ),
    settlement_addr: hubAddressData,
    reserve_addr: hubAddressData,
    payout_addr: hubAddressData,
    reserve_observer: hubPolicy,
  };
  const hubUnit = hubPolicy + fromText("MIDGARD_HUB_ORACLE");
  await submit(
    await operator
      .newTx()
      .collectFrom(await funding())
      .mintAssets({ [hubUnit]: 1n })
      .attach.MintingPolicy(issuer)
      .pay.ToContract(
        hubAddress,
        { kind: "inline", value: Data.to(hubDatum, HubOracleDatum) },
        { lovelace: 10_000_000n, [hubUnit]: 1n },
      )
      .complete({ coinSelection: false, localUPLCEval: true }),
  );
  for (const name of ["deposit", "withdrawal"] as const)
    await submit(
      await operator
        .newTx()
        .collectFrom(await funding())
        .pay.ToAddressWithData(
          hubAddress,
          undefined,
          { lovelace: 100_000_000n },
          pair[name].list.spendingScript,
        )
        .complete({ coinSelection: false, localUPLCEval: true }),
    );
  const published = await operator.utxosAt(hubAddress);
  const referenceScript = (policyId: string): UTxO => {
    const found = published.find(
      (utxo) =>
        utxo.scriptRef != null &&
        validatorToScriptHash(utxo.scriptRef) === policyId,
    );
    if (found === undefined) throw new Error("Reference script is missing");
    return found;
  };
  const referenceScripts = {
    depositMinting: referenceScript(pair.deposit.list.policyId),
    withdrawalMinting: referenceScript(pair.withdrawal.list.policyId),
  };
  let register = operator
    .newTx()
    .collectFrom(await funding())
    .readFrom([
      referenceScripts.depositMinting,
      referenceScripts.withdrawalMinting,
    ]);
  for (const name of ["deposit", "withdrawal"] as const)
    register = register.register.Stake(
      scriptRewardAddress("Custom", pair[name].list.withdrawalScript),
      Data.void(),
    );
  await submit(
    await register.complete({ coinSelection: false, localUPLCEval: true }),
  );
  const start = emulator.now();
  await submit(
    await appendEventHistoryInitialization(
      operator,
      operator.newTx().collectFrom([...(await funding()), nonce]),
      {
        contracts: pair,
        nonce,
        validFrom: start,
        validTo: start + 10_000,
        referenceScripts: {
          deposit: referenceScripts.depositMinting,
          withdrawal: referenceScripts.withdrawalMinting,
        },
      },
    ).complete({ coinSelection: false, localUPLCEval: true }),
  );
  // Past the roots' protection and far enough from the emulator's zero time
  // for an admission's lower bound to trail the clock.
  emulator.awaitSlot(Number(protectionDurationMs / 1000n) + 120);

  const contracts: UserHistoryContracts = {
    hubOracle: {
      policyId: hubPolicy,
      mintingScript: issuer,
      mintingScriptCBOR: issuer.script,
      spendingScript: issuer,
      spendingScriptCBOR: issuer.script,
      spendingScriptHash: hubPolicy,
      spendingScriptAddress: hubAddress,
    },
    eventHistory: pair,
    deposit: pair.deposit.list,
    withdrawal: pair.withdrawal.list,
  };
  const lucid = await lucidFor(user.seedPhrase);
  const view: UserL1View = {
    lucid,
    indexedSlot: async () => emulator.slot,
  };
  /** Program sleeps advance the emulator instead of wall time. */
  const clock = {
    now: () => emulator.now(),
    sleep: async (ms: number) => {
      emulator.awaitSlot(Math.max(1, Math.ceil(ms / 1000)));
    },
  };
  return {
    emulator,
    operator,
    deployer,
    user,
    lucid,
    lucidFor,
    view,
    clock,
    contracts,
    txOrder,
    pair,
    referenceScripts,
    hubPolicy,
    hubAddress,
    issuer,
    initializationNonce: {
      txHash: nonce.txHash,
      outputIndex: nonce.outputIndex,
    },
    protectionDurationMs,
    bounds,
  };
};

const manifestEntry = (
  script: Script,
  scriptHash: string,
  extra: Readonly<Record<string, unknown>> = {},
) => ({
  refScriptUTxO: null,
  contract: { type: script.type, cborHex: script.script },
  scriptHash,
  ...extra,
});

const manifestRecipe = (recipe: EventHistoryRecipe) => ({
  kind: recipe.kind,
  hubPolicyId: recipe.hubPolicyId,
  initializationNonce: {
    txHash: recipe.initializationNonce.transactionId,
    outputIndex: Number(recipe.initializationNonce.outputIndex),
  },
  protectionDurationMs: recipe.protectionDurationMs.toString(),
  bounds: {
    inlineLimitBytes: recipe.inlineLimitBytes.toString(),
    maxPayloadBytes: recipe.maxPayloadBytes.toString(),
    maxPayloadNodes: recipe.maxPayloadNodes.toString(),
  },
});

/**
 * The deployment-manifest sections a user's submission reads, describing
 * this scene exactly: the hub oracle and both lists' scripts with their
 * recipes, the shared initialization nonce, the two minting reference
 * scripts, and the tx-order scripts. A test spreads them over the rest of a manifest.
 */
export const userHistoryManifestSections = (scene: UserHistoryScene) => {
  const contracts: Record<string, ReturnType<typeof manifestEntry>> = {
    hubOracleMint: manifestEntry(scene.issuer, scene.hubPolicy),
  };
  const referenceScripts: Record<
    string,
    {
      status: "confirmed";
      roleUnit: string;
      scriptHash: string;
      outRef: string;
    }
  > = {};
  for (const name of ["deposit", "withdrawal"] as const) {
    const history = scene.pair[name];
    contracts[`${name}Mint`] = manifestEntry(
      history.list.mintingScript,
      history.list.policyId,
      { eventHistoryRecipe: manifestRecipe(history.recipe) },
    );
    contracts[`${name}Spend`] = manifestEntry(
      history.list.spendingScript,
      history.list.spendingScriptHash,
    );
    contracts[`${name}HistoryRetentionSpend`] = manifestEntry(
      history.retention.spendingScript,
      history.retention.spendingScriptHash,
    );
    contracts[`${name}HistoryRetirementWithdraw`] = manifestEntry(
      history.retirement.withdrawalScript,
      history.retirement.withdrawalScriptHash,
    );
    const published =
      name === "deposit"
        ? scene.referenceScripts.depositMinting
        : scene.referenceScripts.withdrawalMinting;
    referenceScripts[`${name} minting`] = {
      status: "confirmed",
      roleUnit: `${scene.hubPolicy}${fromText(`${name} minting`)}`,
      scriptHash: history.list.policyId,
      outRef: `${published.txHash}#${String(published.outputIndex)}`,
    };
  }
  contracts.txOrderMint = manifestEntry(
    scene.txOrder.mintingScript,
    scene.txOrder.policyId,
  );
  contracts.txOrderSpend = manifestEntry(
    scene.txOrder.spendingScript,
    scene.txOrder.spendingScriptHash,
  );
  const nonce = scene.initializationNonce;
  return {
    network: "Custom" as const,
    hubOracleOneShot: {
      txHash: nonce.txHash,
      outputIndex: nonce.outputIndex,
      outRef: `${nonce.txHash}#${String(nonce.outputIndex)}`,
      status: "consumed_by_init" as const,
    },
    contracts,
    referenceScripts,
  };
};
