import {
  type DeploymentManifestCardanoProtocolParameters,
  parseDeploymentManifestCardanoProtocolParameters,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { isPlainFundingOutput } from "@al-ft/midgard-fault-proofs/funding/plain-funding-output";
import { validationDisputeWalletMoveReserve } from "@al-ft/midgard-fault-proofs/workflow/runtime-funding-fee";
import { VALIDATION_CASE_FLOOR_LOVELACE } from "@al-ft/midgard-sdk";
import { paymentCredentialOf, type UTxO } from "@lucid-evolution/lucid";
import { watcherValidationDisputeFundingTerms } from "midgard-watcher";

import type {
  AttachedJourneySession,
  JourneySession,
} from "./journey-session.js";

/**
 * The plain coins the run's actor funding pays the devnet prover, once per
 * run: its collateral and the fee margin of every action it funds.
 */
export const JOURNEY_PROVER_FUNDING_OUTPUTS_LOVELACE: readonly bigint[] =
  Object.freeze([500_000_000n, 500_000_000n, 100_000_000n]);

const JOURNEY_PROVER_FUNDING_LOVELACE =
  JOURNEY_PROVER_FUNDING_OUTPUTS_LOVELACE.reduce(
    (total, lovelace) => total + lovelace,
    0n,
  );

export type ProverDisputeFundingPorts = Readonly<{
  /** The product's custody terms for the family's dispute, if it posts one
   * (`watcherValidationDisputeFundingTerms`). */
  disputeTerms(): Readonly<{
    validationDispute?: Readonly<{ bondLovelace: bigint }>;
  }>;
  /** The deployment's signed protocol parameters. */
  protocolParameters(): DeploymentManifestCardanoProtocolParameters;
  /** The prover's payment key hash. */
  proverKeyHash(): string;
  /** The prover address's outputs, as the watcher's L1 view reads them. */
  readProverUtxos(): Promise<readonly UTxO[]>;
  /** Pays `lovelace` to the prover in one plain coin, and returns once the
   * watcher's L1 view holds it. */
  transfer(lovelace: bigint): Promise<void>;
  onStage(name: string): void;
}>;

/** What the watcher's reservation counts: every plain coin at the prover. */
const spendableLovelace = (utxos: readonly UTxO[]) =>
  utxos
    .filter(isPlainFundingOutput)
    .reduce((total, utxo) => total + (utxo.assets.lovelace ?? 0n), 0n);

/**
 * Before a family whose dispute posts a bond, outside its limit: keeps the
 * prover holding what the watcher's first validation-dispute reservation
 * must hold, the deployment-bound bond, the SDK's case floor and the wallet
 * move reserve (as the watcher's `validationDisputeCustodyLovelace` adds
 * them), over the run's actor funding of the prover. The reservation counts
 * every plain coin at the prover, collateral included, and the same coins
 * pay Init, Open and VerifySource, so a prover holding only the requirement
 * pauses VerifySource; the actor funding stays as that margin. A short
 * prover gets one plain coin of the shortfall, and the step is safe to
 * repeat: a covered prover gets nothing. A family whose dispute posts no
 * bond reads nothing else.
 */
export const ensureProverDisputeFunding = async (
  ports: ProverDisputeFundingPorts,
) => {
  const { validationDispute } = ports.disputeTerms();
  if (validationDispute === undefined) return "posts-no-dispute-bond" as const;
  const requirement =
    validationDispute.bondLovelace +
    VALIDATION_CASE_FLOOR_LOVELACE +
    validationDisputeWalletMoveReserve({
      parameters: ports.protocolParameters(),
      fundingPaymentKeyHash: ports.proverKeyHash(),
    });
  const target = requirement + JOURNEY_PROVER_FUNDING_LOVELACE;
  const available = spendableLovelace(await ports.readProverUtxos());
  if (available >= target) return "covered" as const;
  ports.onStage(
    `prover dispute top-up of ${(target - available).toString()} lovelace (requirement ${requirement.toString()}, margin ${JOURNEY_PROVER_FUNDING_LOVELACE.toString()}, held ${available.toString()})`,
  );
  await ports.transfer(target - available);
  const held = spendableLovelace(await ports.readProverUtxos());
  if (held < target)
    throw new Error(
      `The prover holds ${held.toString()} lovelace after its dispute top-up, short of ${target.toString()}`,
    );
  return "topped-up" as const;
};

/**
 * `ensureProverDisputeFunding` for a session's prover. A devnet pays the
 * shortfall from the run's publisher wallet to the prover the journey
 * launches its watcher with, and waits for it as the run's actor funding
 * does. A stack's prover belongs to the stack, which the drill never funds:
 * a short one refuses the family with its shortfall. A closed session does
 * nothing.
 */
export const proverDisputeTopUpBeforeFamily = async (
  session: JourneySession | AttachedJourneySession,
  input: Readonly<{ category: string; onStage(name: string): void }>,
) => {
  if (session.isClosed()) return "session-closed" as const;
  const { deployment, provider } = session.context;
  const prover =
    session.mode === "attached"
      ? session.context.layout.proverAddress
      : session.proverAddress();
  return ensureProverDisputeFunding({
    disputeTerms: () =>
      watcherValidationDisputeFundingTerms(
        session.authority.deploymentAuthority.deploymentIdentity,
        input.category,
      ),
    protocolParameters: () =>
      parseDeploymentManifestCardanoProtocolParameters(
        deployment.manifest.cardanoProtocolParameters.snapshot,
      ),
    proverKeyHash: () => paymentCredentialOf(prover).hash,
    readProverUtxos: () => provider.getUtxos(prover),
    transfer: async (lovelace) => {
      if (session.mode === "attached")
        throw new Error(
          `The stack watcher's prover ${prover} is ${lovelace.toString()} lovelace short of the '${input.category}' dispute's custody and its margin; fund it before the drill`,
        );
      const { publisherLucid } = deployment;
      const publisherAddress = await publisherLucid.wallet().address();
      publisherLucid.overrideUTxOs(
        (await provider.getUtxos(publisherAddress)).filter(
          isPlainFundingOutput,
        ),
      );
      const built = await publisherLucid
        .newTx()
        .pay.ToAddress(prover, { lovelace })
        .complete({ localUPLCEval: true })
        .finally(() => publisherLucid.clearUTxOOverride());
      const txHash = await (await built.sign.withWallet().complete()).submit();
      input.onStage(`prover dispute top-up ${txHash}`);
      await provider.awaitTx(txHash, 500);
      await session.native.transaction(txHash);
    },
    onStage: input.onStage,
  });
};
