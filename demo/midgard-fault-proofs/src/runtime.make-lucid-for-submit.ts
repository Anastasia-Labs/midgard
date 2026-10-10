/**
 * The fault-proofs prover CLI's submit provider: a Lucid over Blockfrost or
 * Kupmios (owner ruling: the prover CLI keeps both). It is the one module
 * of the package that constructs an external L1 provider, so the role
 * boundary check (`midgard-node/tests/l1-role-boundary.test.ts`) exempts it
 * alone. Only the prover CLI's submit commands call it.
 */
import {
  Blockfrost,
  Kupmios,
  Lucid,
  type LucidEvolution,
  type Network,
  type SlotConfig,
} from "@lucid-evolution/lucid";

export type ProviderKind = "Blockfrost" | "Kupmios";

export type SubmitProviderConfig = {
  readonly slotConfig?: SlotConfig;
  readonly network: Network;
  readonly provider?: ProviderKind;
  readonly blockfrostApiUrl?: string;
  readonly blockfrostKey?: string;
  readonly kupoUrl?: string;
  readonly ogmiosUrl?: string;
};

const normalizeNonEmpty = (value: string | undefined): string | undefined => {
  const trimmed = value?.trim() ?? "";
  return trimmed.length === 0 ? undefined : trimmed;
};

const parseProviderKind = (
  value: string | undefined,
  env: NodeJS.ProcessEnv = process.env,
): ProviderKind => {
  const resolved =
    normalizeNonEmpty(value) ?? normalizeNonEmpty(env.L1_PROVIDER);
  if (resolved === "Blockfrost" || resolved === "Kupmios") {
    return resolved;
  }
  if (resolved === undefined) {
    return "Blockfrost";
  }
  throw new Error('--provider must be either "Blockfrost" or "Kupmios".');
};

const requireConfigValue = (
  direct: string | undefined,
  envName: string,
  label: string,
  env: NodeJS.ProcessEnv,
): string => {
  const resolved = normalizeNonEmpty(direct) ?? normalizeNonEmpty(env[envName]);
  if (resolved === undefined) {
    throw new Error(
      `${label} is required; pass it directly or set ${envName}.`,
    );
  }
  return resolved;
};

export const makeLucidForSubmit = async (
  config: SubmitProviderConfig,
  env: NodeJS.ProcessEnv = process.env,
): Promise<LucidEvolution> => {
  const provider = parseProviderKind(config.provider, env);
  if (provider === "Blockfrost") {
    return await Lucid(
      new Blockfrost(
        requireConfigValue(
          config.blockfrostApiUrl,
          "L1_BLOCKFROST_API_URL",
          "--blockfrost-api-url",
          env,
        ),
        requireConfigValue(
          config.blockfrostKey,
          "L1_BLOCKFROST_KEY",
          "--blockfrost-key",
          env,
        ),
      ),
      config.network,
      { slotConfig: config.slotConfig },
    );
  }

  return await Lucid(
    new Kupmios(
      requireConfigValue(config.kupoUrl, "L1_KUPO_KEY", "--kupo-url", env),
      requireConfigValue(
        config.ogmiosUrl,
        "L1_OGMIOS_KEY",
        "--ogmios-url",
        env,
      ),
    ),
    config.network,
    { slotConfig: config.slotConfig },
  );
};
