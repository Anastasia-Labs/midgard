/**
 * A validation case deployment on the emulator: the case validator (mint and
 * spend under one hash, the thread policy K), its configuration, the thread
 * expiry, the award and the bond, with the thread holders over K. Every
 * Plutus script is read from a published reference script.
 *
 * The surroundings are native stand-ins the wallet signs for: the
 * computation-thread policy, the hub oracle policy, the active-operators
 * policy and node holder, the fraud-proof policy and address, and the
 * source address the case configuration names. The source stand-in is the
 * VerifySource seam: it opens a case with any datum the test gives it,
 * where source-v1 would derive that datum from the challenged block.
 */
import {
  MIDGARD_VALIDATION_DISPUTE_SIDE_CLOCK_MS,
  MIDGARD_VALIDATION_DISPUTE_THREAD_FEE_RESERVE_LOVELACE,
} from "@al-ft/midgard-core";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import {
  appendValidationCaseConfigInitialization,
  buildValidationCaseContracts,
  HUB_ORACLE_ASSET_NAME,
  parseFaultProofBlueprint,
  protocolTreasuryAddress,
  VALIDATION_CASE_CONFIG_ASSET_NAME,
  validationCaseConfigAddress,
} from "@al-ft/midgard-sdk";
import {
  Constr,
  Data,
  type Native,
  paymentCredentialOf,
  type Script,
  scriptFromNative,
  type TxBuilder,
  type TxSignBuilder,
  type UTxO,
  validatorToAddress,
  validatorToRewardAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import {
  type CompleteSignedTransactionMeasurement,
  measureCompleteSignedTransaction,
  network,
  publishPlainReferenceScriptUtxo,
  readBlueprint,
  realBlueprintPath,
} from "./submit-init-emulator-shared.js";
import {
  cap,
  createdAt,
  deploy,
  type DeployedScript,
  deployThreadHolders,
  fraudProofOutputLovelace,
  type GameScene,
  newEmulator,
  newKey,
  scriptAddress,
  TRACE_TITLES,
} from "./validation-game-scene.js";

/** The bond every case of this deployment locks. */
export const bondLovelace = 1_000_000_000n;
/** What a VerifySource leaves in the case: the fraud-proof output and a fee. */
export const caseFloorLovelace = fraudProofOutputLovelace + cap;
/** What a Defend leaves in the thread: the receipt output and the reserve. */
export const threadFloorLovelace =
  fraudProofOutputLovelace +
  MIDGARD_VALIDATION_DISPUTE_THREAD_FEE_RESERVE_LOVELACE;
export const sideClockMs = MIDGARD_VALIDATION_DISPUTE_SIDE_CLOCK_MS;

const timing = SELECTED_DEPLOYMENT_PROFILE.timing;

/** The last Defend lower bound: `started_at + C + L_d` (`defense_deadline`). */
export const defenseDeadline = (startedAt: number) =>
  startedAt + sideClockMs + timing.dispute_max_validity_range_ms;

/** `started_at + D_case + P`: the bond hold a defense needs. */
export const defenseBondUnlockTime = (startedAt: number) =>
  startedAt +
  timing.block_maturity_ms -
  Math.floor(timing.block_maturity_ms / 2);

export const treasuryAddress = () =>
  protocolTreasuryAddress(
    network,
    SELECTED_DEPLOYMENT_PROFILE.protocol_treasury_native_script,
  );

/** A native script only the wallet satisfies, distinct for each `depth`. */
const walletScript = (walletKey: string, depth: number): DeployedScript => {
  let native: Native = { type: "sig", keyHash: walletKey };
  for (let level = 0; level < depth; level += 1) {
    native = { type: "all", scripts: [native] };
  }
  const script = scriptFromNative(native);
  return {
    script,
    hash: validatorToScriptHash(script),
    address: validatorToAddress(network, script),
  };
};

export type CaseWorld = GameScene & {
  /** The case validator: its hash is K, the case and thread policy. */
  readonly caseValidator: DeployedScript;
  readonly award: DeployedScript;
  readonly bond: DeployedScript;
  /** The thread expiry: its script, hash and registered reward address. */
  readonly threadExpiry: {
    readonly script: Script;
    readonly hash: string;
    readonly reward: string;
  };
  /** The reference inputs holding the configuration and the hub NFTs. */
  readonly config: UTxO;
  readonly hub: UTxO;
  readonly computationThreadPolicy: DeployedScript;
  readonly activeOperatorsPolicy: DeployedScript;
  readonly nodeHolder: DeployedScript;
  readonly source: DeployedScript;
  readonly fraudProofPolicy: DeployedScript;
  readonly fraudProofAddress: string;
  /** Complete signed bytes of each reference-script publication, by title. */
  readonly publications: ReadonlyMap<string, number>;
};

/** Signs with the wallet and any extra keys, submits and measures. */
export const submitSigned = async (
  world: Pick<GameScene, "lucid">,
  built: TxSignBuilder,
  signingKeys: readonly string[] = [],
): Promise<{
  readonly hash: string;
  readonly budget: CompleteSignedTransactionMeasurement;
}> => {
  let signing = built.sign.withWallet();
  for (const key of signingKeys) signing = signing.sign.withPrivateKey(key);
  const signed = await signing.complete();
  const budget = measureCompleteSignedTransaction(signed.toCBOR());
  const hash = await signed.submit();
  await world.lucid.awaitTx(hash);
  return { hash, budget };
};

const submitBuilder = async (world: Pick<GameScene, "lucid">, tx: TxBuilder) =>
  (await submitSigned(world, await tx.complete({ localUPLCEval: true }))).hash;

const addressData = (scriptHash: string) =>
  new Constr(0, [new Constr(1, [scriptHash]), new Constr(1, [])]);

/**
 * The hub oracle datum, its 26 fields in order. The case reads three: the
 * active-operators policy (1), the fraud-proof policy (6) and the fraud-proof
 * address (18).
 */
const hubDatum = (
  activeOperatorsPolicyId: string,
  fraudProofPolicyId: string,
  fraudProofAddressHash: string,
) =>
  Data.to(
    new Constr(
      0,
      Array.from(
        { length: 26 },
        (_, index): Data =>
          index === 1
            ? activeOperatorsPolicyId
            : index === 6
              ? fraudProofPolicyId
              : index === 18
                ? addressData(fraudProofAddressHash)
                : 0n,
      ),
    ),
  );

export const setUpCaseWorld = async (): Promise<CaseWorld> => {
  const { wallet, emulator, lucid } = await newEmulator();
  const prover = newKey();
  const walletKey = paymentCredentialOf(wallet.address).hash;
  const computationThreadPolicy = walletScript(walletKey, 1);
  const hubPolicy = walletScript(walletKey, 2);
  const activeOperatorsPolicy = walletScript(walletKey, 3);
  const nodeHolder = walletScript(walletKey, 4);
  const source = walletScript(walletKey, 5);
  const fraudProofPolicy = walletScript(walletKey, 6);
  const fraudProofHolder = walletScript(walletKey, 7);
  const nonceHolder = walletScript(walletKey, 8);
  const park = scriptAddress("2f".repeat(28));

  const nonce = await createdAt(
    { lucid },
    await submitBuilder(
      { lucid },
      lucid
        .newTx()
        .pay.ToAddress(nonceHolder.address, { lovelace: 5_000_000n }),
    ),
    0,
  );
  const contracts = buildValidationCaseContracts({
    blueprint: parseFaultProofBlueprint(readBlueprint(realBlueprintPath)),
    network,
    hubOraclePolicyId: hubPolicy.hash,
    computationThreadPolicyId: computationThreadPolicy.hash,
    disputeBondLovelace: bondLovelace,
    caseConfigInitializationNonce: nonce,
  });
  const threadPolicyId = contracts.case.policyId;
  const threadExpiry = {
    script: contracts.threadExpiry.withdrawalScript,
    hash: contracts.threadExpiry.withdrawalScriptHash,
    reward: validatorToRewardAddress(
      network,
      contracts.threadExpiry.withdrawalScript,
    ),
  };
  const award: DeployedScript = {
    script: contracts.award.spendingScript,
    hash: contracts.award.spendingScriptHash,
    address: contracts.award.spendingScriptAddress,
  };
  const caseValidator: DeployedScript = {
    script: contracts.case.spendingScript,
    hash: contracts.case.spendingScriptHash,
    address: contracts.case.spendingScriptAddress,
  };
  const holders = deployThreadHolders(
    threadPolicyId,
    threadExpiry.hash,
    award.hash,
  );
  const bond = deploy(TRACE_TITLES.bond, [threadPolicyId]);

  const scriptReferences = new Map<string, UTxO>();
  const publications = new Map<string, number>();
  const published = [
    ["case_config_v1", contracts.caseConfig.mintingScript],
    ["case_v1", caseValidator.script],
    ["award_v1", award.script],
    ["game_v1", holders.game.script],
    ["boundary_v1", holders.boundary.script],
    ["timeout_v1", holders.timeout.script],
    ["dispute_bond_v1", bond.script],
    ["thread_expiry_v1", threadExpiry.script],
  ] as const;
  for (const [label, script] of published) {
    const { utxo, publicationMeasurement } =
      await publishPlainReferenceScriptUtxo({ lucid, script, label });
    scriptReferences.set(validatorToScriptHash(script), utxo);
    publications.set(label, publicationMeasurement.completeSignedBytes);
  }

  await submitBuilder(
    { lucid },
    lucid.newTx().register.Stake(threadExpiry.reward),
  );

  const hubUnit = hubPolicy.hash + HUB_ORACLE_ASSET_NAME;
  const hub = await createdAt(
    { lucid },
    await submitBuilder(
      { lucid },
      lucid
        .newTx()
        .mintAssets({ [hubUnit]: 1n })
        .attach.MintingPolicy(hubPolicy.script)
        .pay.ToContract(
          park,
          {
            kind: "inline",
            value: hubDatum(
              activeOperatorsPolicy.hash,
              fraudProofPolicy.hash,
              fraudProofHolder.hash,
            ),
          },
          { [hubUnit]: 1n },
        ),
    ),
    0,
  );

  const configTx = appendValidationCaseConfigInitialization(
    lucid
      .newTx()
      .collectFrom([nonce])
      .attach.SpendingValidator(nonceHolder.script),
    {
      network,
      contracts: {
        caseConfig: contracts.caseConfig,
        source: { spendingScriptHash: source.hash },
        game: { spendingScriptHash: holders.game.hash },
        boundary: { spendingScriptHash: holders.boundary.hash },
        award: contracts.award,
        timeout: { spendingScriptHash: holders.timeout.hash },
        disputeBond: { spendingScriptHash: bond.hash },
        threadExpiry: contracts.threadExpiry,
      },
      referenceScript: scriptReferences.get(contracts.caseConfig.policyId),
    },
  );
  await submitBuilder({ lucid }, configTx);
  const [config] = await lucid.utxosAtWithUnit(
    validationCaseConfigAddress(network, contracts.caseConfig),
    contracts.caseConfig.policyId + VALIDATION_CASE_CONFIG_ASSET_NAME,
  );

  return {
    lucid,
    emulator,
    wallet,
    proverSigningKey: prover.signingKey,
    proverKey: prover.hash,
    proverAddress: prover.address,
    threadPolicyId,
    ...holders,
    scriptReferences,
    caseValidator,
    award,
    bond,
    threadExpiry,
    config: config!,
    hub,
    computationThreadPolicy,
    activeOperatorsPolicy,
    nodeHolder,
    source,
    fraudProofPolicy,
    fraudProofAddress: fraudProofHolder.address,
    publications,
  };
};

/** The script reference for `script`, which the world published. */
export const referenceOf = (world: CaseWorld, script: DeployedScript) => {
  const reference = world.scriptReferences.get(script.hash);
  if (reference === undefined) throw new Error("unpublished script");
  return reference;
};
