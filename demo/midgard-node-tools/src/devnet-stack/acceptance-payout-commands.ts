import { registerFinalAcceptance } from "./acceptance-cli.js";
import { registerAcceptancePayouts } from "./acceptance-payout-cli.js";

export const registerAcceptanceCommands = (
  ...args: Parameters<typeof registerFinalAcceptance>
) => {
  registerFinalAcceptance(...args);
  registerAcceptancePayouts(args[0]);
};
