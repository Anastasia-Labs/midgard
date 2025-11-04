import { Network } from "@lucid-evolution/lucid";

export type ProtocolParameters = {
  event_wait_duration: number;
};

/**
 * Given the network, this functions returns appropriate protocol parameters.
 */
export const getProtocolParameters = (network: Network): ProtocolParameters => {
  if (network === "Mainnet") {
    return {
      event_wait_duration: 60_000,
    };
  } else {
    return {
      event_wait_duration: 50_000,
    };
  }
};
