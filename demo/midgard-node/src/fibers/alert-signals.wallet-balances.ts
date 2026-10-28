/**
 * Wallet balance gauges for the alert rules
 * (`demo/midgard-node/alert-rules.yml`, `MidgardWalletBalanceLow`): the
 * plain-ADA lovelace in each of the node's role wallets, and, for the roles
 * that must hold a known amount, that amount. Both are labelled by role
 * only, never by address.
 *
 * A balance is read from the role wallet's view (plan §8.5) through the
 * intent journal, the view every node builder selects its inputs from. Under
 * `listen` the journal is the follower's, so the view is a read of the node
 * database: sampling it adds no L1 provider read.
 *
 * What each role must hold, from what its builders need:
 *
 * - `operator`: the defense reserve
 *   `demo/midgard-node/docs/validation-trace-defense.md` asks for (two
 *   Defends' thread floors, the pocket's collateral output and a Defend's
 *   margin) above one commit's new block node, which every commit pays.
 * - `merge` and `settlement`: no amount of their own; each signs Plutus
 *   transactions that present Lucid's default collateral from the wallet,
 *   so they must hold at least that collateral.
 * - `reference_script` and `defense_pocket`: none. Publication tops the
 *   reference-script wallet up from the operator wallet before it spends,
 *   and a Defend pays the pocket its collateral output when it holds none.
 *   Their balances are exported for the operator to read, with no rule.
 */
import {
  isPlainFundingOutput,
  VALIDATION_TRACE_POCKET_COLLATERAL_LOVELACE,
  VALIDATION_TRACE_THREAD_FLOOR_LOVELACE,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";
import { Effect, Metric } from "effect";

import {
  type NodeConfigDep,
  seedPhraseAddress,
} from "../services/config.node-config-dep.js";
import { IntentJournal } from "../services/intent-journal.js";
import { defensePocketAddress } from "../services/intent-journal.tracked-set.js";
import type { WalletViewUnavailable } from "../services/intent-journal.wallet-view.js";
import { VALIDATION_TRACE_DEFEND_MARGIN_LOVELACE } from "../validation-trace-defense/funding.js";

export type WalletRole =
  | "operator"
  | "merge"
  | "settlement"
  | "reference_script"
  | "defense_pocket";

export type RoleWallet = Readonly<{
  role: WalletRole;
  address: string;
  /** The plain-ADA lovelace the role must hold; none for a role with no
   * requirement of its own. */
  requiredLovelace?: bigint;
}>;

/**
 * The operator wallet's requirement: the defense reserve
 * (2 × thread floor + pocket collateral + Defend margin) above one commit's
 * new block node.
 */
export const OPERATOR_WALLET_REQUIRED_LOVELACE =
  2n * VALIDATION_TRACE_THREAD_FLOOR_LOVELACE +
  VALIDATION_TRACE_POCKET_COLLATERAL_LOVELACE +
  VALIDATION_TRACE_DEFEND_MARGIN_LOVELACE +
  SDK.STATE_QUEUE_NODE_MIN_LOVELACE;

/**
 * The collateral Lucid's `complete` presents for a Plutus transaction built
 * without `setCollateral` (its default; more only when the fee's collateral
 * percentage exceeds it). Every merge and settlement transaction is built so.
 */
export const LUCID_DEFAULT_COLLATERAL_LOVELACE = 5_000_000n;

/** The node's role wallets, by role, from its configured seeds. */
export const nodeRoleWallets = (
  config: Pick<
    NodeConfigDep,
    | "NETWORK"
    | "L1_OPERATOR_SEED_PHRASE"
    | "L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX"
    | "L1_SETTLEMENT_SEED_PHRASE"
    | "L1_REFERENCE_SCRIPT_SEED_PHRASE"
  >,
): readonly RoleWallet[] => {
  const settlementSeed = config.L1_SETTLEMENT_SEED_PHRASE?.trim() ?? "";
  return [
    {
      role: "operator",
      address: seedPhraseAddress(
        config.L1_OPERATOR_SEED_PHRASE,
        config.NETWORK,
      ),
      requiredLovelace: OPERATOR_WALLET_REQUIRED_LOVELACE,
    },
    {
      role: "merge",
      address: seedPhraseAddress(
        config.L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX,
        config.NETWORK,
      ),
      requiredLovelace: LUCID_DEFAULT_COLLATERAL_LOVELACE,
    },
    ...(settlementSeed === ""
      ? []
      : [
          {
            role: "settlement",
            address: seedPhraseAddress(settlementSeed, config.NETWORK),
            requiredLovelace: LUCID_DEFAULT_COLLATERAL_LOVELACE,
          } as const,
        ]),
    {
      role: "reference_script",
      address: seedPhraseAddress(
        config.L1_REFERENCE_SCRIPT_SEED_PHRASE,
        config.NETWORK,
      ),
    },
    { role: "defense_pocket", address: defensePocketAddress(config) },
  ];
};

const walletPlainLovelaceGauge = Metric.gauge("midgard_wallet_plain_lovelace", {
  description:
    "Lovelace in the plain-ADA outputs (no token, datum or script reference) of a node role wallet's view, by role",
  bigint: true,
});

const walletRequiredLovelaceGauge = Metric.gauge(
  "midgard_wallet_required_lovelace",
  {
    description:
      "Plain-ADA lovelace a node role wallet must hold for what its transactions need, by role; absent for a role with no requirement",
    bigint: true,
  },
);

/**
 * Publishes each role wallet's plain-ADA balance, read from its view, and
 * its requirement. A view that cannot be read fails the sample; the gauges
 * read so far keep their values.
 */
export const sampleWalletBalances = (
  lucid: LucidEvolution,
  wallets: readonly RoleWallet[],
): Effect.Effect<void, WalletViewUnavailable, IntentJournal> =>
  Effect.flatMap(IntentJournal, (journal) =>
    Effect.forEach(
      wallets,
      ({ role, address, requiredLovelace }) =>
        Effect.gen(function* () {
          const view = yield* journal.walletView(lucid, address);
          const plain = view.utxos
            .filter(isPlainFundingOutput)
            .reduce((sum, utxo) => sum + (utxo.assets.lovelace ?? 0n), 0n);
          yield* Metric.tagged(
            walletPlainLovelaceGauge,
            "role",
            role,
          )(Effect.succeed(plain));
          if (requiredLovelace !== undefined)
            yield* Metric.tagged(
              walletRequiredLovelaceGauge,
              "role",
              role,
            )(Effect.succeed(requiredLovelace));
        }),
      { discard: true },
    ),
  );
