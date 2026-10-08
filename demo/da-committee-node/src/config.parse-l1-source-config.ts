import {
  type CardanoL1SourceConfig,
  type Env,
} from "./config.committee-config.js";
import { optionalNonEmpty } from "./config.operational-provider-identity.js";

const CARDANO_NAMED_NETWORK_MAGIC = {
  Mainnet: 764_824_073,
  Preprod: 1,
  Preview: 2,
} as const;

const CARDANO_NETWORK_MAGIC_MAX = 4_294_967_295;

/** The network magic of the committee's node: named networks fix it, `Custom` reads `CARDANO_NETWORK_MAGIC`. */
export const cardanoL1SourceConfig = ({
  env,
  network,
}: {
  readonly env: Env;
  readonly network: string;
}): CardanoL1SourceConfig => ({
  networkMagic: cardanoNetworkMagic(env, network),
});

const cardanoNetworkMagic = (env: Env, network: string): number => {
  const configured = optionalNonEmpty(env.CARDANO_NETWORK_MAGIC);
  if (network === "Custom") {
    if (configured === undefined) {
      throw new Error("CARDANO_NETWORK_MAGIC is required for Custom network");
    }
    return networkMagicInteger(configured);
  }
  if (network === "Mainnet" || network === "Preprod" || network === "Preview") {
    if (configured !== undefined) {
      throw new Error(
        "CARDANO_NETWORK_MAGIC must be omitted for named Cardano networks",
      );
    }
    return CARDANO_NAMED_NETWORK_MAGIC[network];
  }
  throw new Error(
    "Cardano network must be Mainnet, Preprod, Preview, or Custom",
  );
};

const networkMagicInteger = (value: string): number => {
  if (!/^(?:0|[1-9][0-9]*)$/u.test(value)) {
    throw new Error(
      "CARDANO_NETWORK_MAGIC must be a canonical unsigned 32-bit integer",
    );
  }
  const parsed = Number(value);
  if (
    !Number.isSafeInteger(parsed) ||
    parsed < 0 ||
    parsed > CARDANO_NETWORK_MAGIC_MAX
  ) {
    throw new Error(
      "CARDANO_NETWORK_MAGIC must be a canonical unsigned 32-bit integer",
    );
  }
  return parsed;
};
