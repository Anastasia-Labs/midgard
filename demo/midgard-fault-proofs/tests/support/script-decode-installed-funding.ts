/**
 * The watcher's prover funding for one installed objective over an emulator
 * deployment: the real runtime funding policy, the real watcher reservation
 * authority and the production SQLite reservation store.
 *
 * Only the roster's source differs from the watcher's factory: the factory
 * derives it from the signed manifest's applied script hashes, which name
 * the fixture manifest's scripts, not the emulator's. The roster here is the
 * emulator deployment's, mapped to custody roles exactly as the factory maps
 * them. Each mint first passes the factory's recovery authorization over the
 * watcher's journal root. Given the node's parameters at each turn, a mint
 * prices its policy at them, while an existing reservation keeps the basis
 * it was admitted on. The factory differs in one place: it checks an
 * existing reservation's capacity at the parameters it remembered when the
 * reservation was made, where this checks it at the parameters of the turn.
 */
import { join } from "node:path";

import {
  credentialToAddress,
  type Emulator,
  type LucidEvolution,
  scriptHashToCredential,
  type UTxO,
  utxoToCore,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import {
  calculateWatcherRuntimeProverFunding,
  createWatcherProtocolParameterRuntimeAuthority,
  createWatcherProverFundingAuthority,
  openWatcherSqliteProverFundingReservationStore,
  type VerifiedWatcherDeploymentIdentity,
} from "midgard-watcher";
import {
  authorizeWatcherProverFundingRecovery,
  TEST_JOURNAL_KEY,
} from "midgard-watcher/tests/support/installed-prover-funding-recovery";
import { ledgerParameterQuery } from "midgard-watcher/tests/support/ledger-protocol-parameters";

import type { WorkflowActuationPermit } from "../../src/workflow/actuation-permit.js";
import type { WorkflowAdapterRunner } from "../../src/workflow/adapters.js";
import { createWorkflowRuntimeFundingPolicy } from "../../src/workflow/runtime-funding-policy.js";
import { SCRIPT_DECODE_SPEND_ROLE } from "../../src/workflow/script-decode-prerequisite.js";

type Category = Parameters<
  typeof createWorkflowRuntimeFundingPolicy
>[0]["category"];
type Economics = Parameters<
  typeof createWorkflowRuntimeFundingPolicy
>[0]["economics"];

/** `value` as the exact decimal rational a node's ledger answer carries. */
const rational = (value: number): readonly [bigint, bigint] => {
  for (let digits = 0; digits <= 15; digits += 1) {
    const denominator = 10 ** digits;
    const numerator = Math.round(value * denominator);
    if (numerator / denominator === value)
      return [BigInt(numerator), BigInt(denominator)];
  }
  throw new Error(`protocol parameter ${value.toString()} is not decimal`);
};

/**
 * The local node's protocol-parameter authority, answering the emulator's
 * own parameters, so funding bounds are those the ledger enforces: `lucid`'s
 * at the authority's creation, or, given `live`, that emulator's at each
 * query, so the watcher's funding follows a change of its fees.
 */
export const emulatorProtocolParameterAuthority = async (
  identity: VerifiedWatcherDeploymentIdentity,
  lucid: LucidEvolution,
  live?: Emulator,
) => {
  const booted = lucid.config().protocolParameters;
  if (booted === undefined)
    throw new Error("emulator Lucid has no protocol parameters");
  const query = async () => {
    const parameters = live?.protocolParameters ?? booted;
    return await ledgerParameterQuery({
      minFeeA: BigInt(parameters.minFeeA),
      minFeeB: BigInt(parameters.minFeeB),
      maxTxSize: BigInt(parameters.maxTxSize),
      coinsPerUtxoByte: BigInt(parameters.coinsPerUtxoByte),
      priceMemory: rational(parameters.priceMem),
      priceSteps: rational(parameters.priceStep),
      maxTxExUnits: [
        BigInt(parameters.maxTxExMem),
        BigInt(parameters.maxTxExSteps),
      ],
      maxValueSize: BigInt(parameters.maxValSize),
      collateralPercentage: BigInt(parameters.collateralPercentage),
      maxCollateralInputs: BigInt(parameters.maxCollateralInputs),
      minFeeRefScriptCostPerByte: rational(
        parameters.minFeeRefScriptCostPerByte,
      ),
    })();
  };
  return await createWatcherProtocolParameterRuntimeAuthority({
    deploymentIdentity: identity,
    query,
  });
};

/** The watcher factory's custody role for a deployed contract name. */
const custodyRole = (name: string) =>
  name === "correctionLockSpend"
    ? ("correction_lock" as const)
    : [
          "fraudProofSpend",
          "fraudProofCatalogueSpend",
          "fieldPreimageCertificateSpend",
          SCRIPT_DECODE_SPEND_ROLE,
          "validationTraceDisputeProofItem",
        ].includes(name)
      ? ("field_carrier" as const)
      : (name.startsWith("fraudProof") &&
            !name.startsWith("fraudProofCatalogue")) ||
          name.startsWith("validationTraceDispute")
        ? ("proof_thread" as const)
        : name.endsWith("Spend")
          ? ("protocol_state" as const)
          : undefined;

const outRefOf = (outRef: string) => {
  const [txHash, index] = outRef.split("#");
  return { txHash: txHash!, outputIndex: Number(index) };
};

/**
 * Opens the reservation store in `directory` and returns the supervisor's
 * per-turn permit mint (`mintWatcherProverFundingReservationPermit`'s body)
 * for one objective. Reopening the same directory is a watcher restart.
 */
export const openInstalledProverFunding = async ({
  emulator,
  identity,
  category,
  runner,
  economics,
  protocolParameters,
  fundingPaymentKeyHash,
  walletAddress,
  computationThreadPolicyId,
  scriptHashes,
  referenceUtxos,
  directory,
  journalRoot,
  validationDispute,
  currentParameters,
}: {
  readonly emulator: Emulator;
  readonly identity: VerifiedWatcherDeploymentIdentity;
  readonly category: Category;
  readonly runner: WorkflowAdapterRunner;
  readonly economics: Economics;
  readonly protocolParameters: Awaited<
    ReturnType<typeof emulatorProtocolParameterAuthority>
  >;
  readonly fundingPaymentKeyHash: string;
  readonly walletAddress: string;
  /** The deployed computation-thread policy (the watcher derives it from the
   * blueprint); it proves own thread custody whose lineage was lost. */
  readonly computationThreadPolicyId?: string;
  /** Deployed contract name to script hash, as the manifest lists them. */
  readonly scriptHashes: Readonly<Record<string, string>>;
  /** Every published reference UTxO a workflow transaction may read. */
  readonly referenceUtxos: readonly UTxO[];
  /** Durable home of the SQLite reservation store. */
  readonly directory: string;
  /**
   * The watcher's journal root, where the fault decisions are journaled.
   * Null for a category the watcher does not install, which has no recovery
   * authorization: its decisions are never journaled.
   */
  readonly journalRoot: string | null;
  /** A validation dispute's deployment-bound VerifySource custody terms. */
  readonly validationDispute?: Parameters<
    typeof createWorkflowRuntimeFundingPolicy
  >[0]["validationDispute"];
  /** The node's parameters at a turn; `protocolParameters` at every turn
   * when absent. */
  readonly currentParameters?: () => ReturnType<
    typeof emulatorProtocolParameterAuthority
  >;
}) => {
  const fundingContracts = new Map<
    string,
    Readonly<{
      address: string;
      scriptHash: string;
      role: NonNullable<ReturnType<typeof custodyRole>>;
    }>
  >();
  for (const [name, scriptHash] of Object.entries(scriptHashes)) {
    const role =
      name.endsWith("Mint") || name.endsWith("Withdraw")
        ? undefined
        : custodyRole(name);
    if (role === undefined) continue;
    const address = credentialToAddress(
      "Custom",
      scriptHashToCredential(scriptHash),
    );
    if (!fundingContracts.has(address))
      fundingContracts.set(address, { address, scriptHash, role });
  }
  const references = new Map<string, { outRef: string; scriptHash: string }>();
  for (const utxo of referenceUtxos)
    if (utxo.scriptRef != null) {
      const outRef = `${utxo.txHash}#${utxo.outputIndex.toString()}`;
      references.set(outRef, {
        outRef,
        scriptHash: validatorToScriptHash(utxo.scriptRef),
      });
    }
  /** The policy and its funding calculation at `parameters`. */
  const basisAt = async (parameters: typeof protocolParameters) => {
    const policy = createWorkflowRuntimeFundingPolicy({
      category,
      runner,
      deploymentFingerprint: identity.manifestId,
      fundingPaymentKeyHash,
      protocolParameters: parameters.snapshot,
      economics,
      contracts: [...fundingContracts.values()],
      referenceScripts: [...references.values()],
      ...(validationDispute === undefined ? {} : { validationDispute }),
    });
    const calculation = await calculateWatcherRuntimeProverFunding({
      deploymentIdentity: identity,
      protocolParameters: parameters,
      policy,
    });
    return { parameters, policy, calculation } as const;
  };
  const booted = await basisAt(protocolParameters);
  /** Every basis a mint has priced at, by its policy digest: a
   * reservation's own is the one it was admitted on. */
  const bases = new Map([[booted.calculation.policyDigest, booted]]);
  /** The basis at the node's parameters now. */
  const currentBasis = async () => {
    const parameters = (await currentParameters?.()) ?? protocolParameters;
    const known = [...bases.values()].find(
      (basis) => basis.parameters.snapshotDigest === parameters.snapshotDigest,
    );
    if (known !== undefined) return known;
    const basis = await basisAt(parameters);
    bases.set(basis.calculation.policyDigest, basis);
    return basis;
  };
  const opened = await openWatcherSqliteProverFundingReservationStore({
    path: join(directory, "watcher.sqlite"),
    protocolParameterHistory: {
      deploymentIdentity: identity,
      authenticationKey: Buffer.alloc(32, 0x91),
    },
  });
  const readWalletUtxos = async () => await emulator.getUtxos(walletAddress);
  const mintPermit = async ({
    actuationPermit,
    decisionDigest,
  }: {
    readonly actuationPermit: WorkflowActuationPermit;
    readonly decisionDigest: string;
  }) => {
    if (journalRoot !== null)
      await authorizeWatcherProverFundingRecovery({
        journalRoot,
        journalAuthenticationKey: TEST_JOURNAL_KEY,
        workflowJournal: opened.journal,
        deploymentIdentity: identity,
        actuationPermit,
        category,
        rollbackGeneration: "0",
        store: opened.store,
      });
    const existing = (await opened.store.readAll()).find(
      (record) =>
        (record as { readonly decisionDigest?: unknown }).decisionDigest ===
        decisionDigest,
    ) as { readonly policyDigest?: unknown } | undefined;
    const current = await currentBasis();
    // An existing lease keeps its admitted basis; a new one is admitted on
    // today's parameters.
    const admitted =
      existing === undefined
        ? current
        : (bases.get(String(existing.policyDigest)) ?? booted);
    return (
      await createWatcherProverFundingAuthority({
        category,
        runner,
        actuationPermit,
        rollbackGeneration: "0",
        deploymentIdentity: identity,
        calculation: admitted.calculation,
        selectionCalculation: current.calculation,
        policy: current.policy,
        reservationPolicy: admitted.policy,
        capacityPolicy: current.policy,
        decisionDigest,
        walletAddress,
        walletUtxos: existing === undefined ? await readWalletUtxos() : [],
        readWalletUtxos,
        store: opened.store,
        resolveInputs: async (outRefs) =>
          await emulator.getUtxosByOutRef(outRefs.map(outRefOf)),
        computationThreadPolicyId,
        resolveProtocolInputAuthority: async ({
          deploymentIdentity,
          outRef,
          semanticRole,
        }) => {
          const resolved = await emulator.getUtxosByOutRef([outRefOf(outRef)]);
          if (resolved.length !== 1)
            throw new Error("protocol input is not a unique live output");
          return Object.freeze({
            deploymentFingerprint: deploymentIdentity.manifestId,
            outRef,
            semanticRole,
            resolvedOutputCborHex: utxoToCore(resolved[0]!)
              .output()
              .to_canonical_cbor_hex(),
          });
        },
      })
    ).permit;
  };
  return {
    mintPermit,
    readReservations: async () => await opened.store.readAll(),
    /** The per-proof workflow journal in the same store. */
    workflowJournal: opened.journal,
    close: () => {
      opened.close();
    },
  } as const;
};
