/**
 * The tools CLI's typed environment edge.
 *
 * Almost everything in this package already takes the environment as an
 * injected `env` parameter defaulting to `process.env` — `stress-wallets`,
 * `stress-open-loop`, `phase4-genesis-ledger`, `e2e-process-cleanup` and the
 * rest — which is the shape to keep: it is what makes their parsing testable
 * without mutating the real process. The exceptions were a handful of reads
 * inlined into `src/index.ts` where the command wiring is assembled, far from
 * anything that could validate them. Those live here now, in the same
 * injectable shape.
 *
 * The per-command gates in `commands/e2e-pipelined-commit-process-acceptance.ts`
 * are deliberately not moved: they are that one command's authorization
 * preconditions, they are already parsed through local `requiredEnv` and
 * `positiveIntegerEnv` helpers, and hoisting them here would put a destructive
 * devnet run's arming switches somewhere unrelated code could reach.
 */

export type L1KupmiosEnvironment = {
  readonly provider: string | undefined;
  readonly providerFailover: string | undefined;
  readonly kupoUrl: string;
  readonly ogmiosUrl: string;
};

/**
 * The four L1 provider variables, read exactly as the local state-correction
 * authority consumes them: the provider names stay optional strings because the
 * consumer normalizes them itself (it treats blank and `"false"` as off), and
 * the two URLs collapse an unset value to `""`.
 *
 * `midgard-node`'s own `services/config.ts` reads the same four through Effect
 * `Config` and rejects a provider other than `Kupmios`. This reader is
 * deliberately more permissive, because the tools CLI passes them through to a
 * summary rather than driving a node with them.
 */
export const l1KupmiosEnvironment = (
  env: NodeJS.ProcessEnv = process.env,
): L1KupmiosEnvironment => ({
  provider: env.L1_PROVIDER,
  providerFailover: env.L1_PROVIDER_FAILOVER,
  kupoUrl: env.L1_KUPO_KEY ?? "",
  ogmiosUrl: env.L1_OGMIOS_KEY ?? "",
});

export type StressNetwork = "Mainnet" | "Preprod";

/**
 * The network the L2 throughput stress run reports itself as targeting.
 *
 * Only `Mainnet` is recognized; every other value, including `Preview` and
 * `Custom`, reads as `Preprod`. That is the behaviour this had when it was
 * inlined, and it is preserved here rather than tightened, because the value
 * only labels a stress summary. It is stated explicitly so the narrowing is a
 * decision on the page instead of a silent `?:` in the middle of an options
 * object.
 */
export const stressNetworkFromEnvironment = (
  env: NodeJS.ProcessEnv = process.env,
): StressNetwork => (env.NETWORK === "Mainnet" ? "Mainnet" : "Preprod");
