import { Network } from "@lucid-evolution/lucid";
import { PosixTimeDuration } from "./common.js";

//TODO: change event_wait_duration to POSIXTime or maturity_duration to number for better consistency
export type ProtocolParameters = {
  event_wait_duration: number;
  maturity_duration: PosixTimeDuration 
};

/**
 * Given the network, this functions returns appropriate protocol parameters.
 */
export const getProtocolParameters = (network: Network): ProtocolParameters => {
  if (network === "Mainnet") {
    return {
      event_wait_duration: 60_000,
      maturity_duration: 30n 
    };
  } else {
    return {
      event_wait_duration: 50_000,
      maturity_duration: 1n, 
    };
  }
};
