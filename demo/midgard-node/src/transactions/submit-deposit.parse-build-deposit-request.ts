import { type BuildDepositRequest } from "./submit-deposit.deposit-submission-attempt-from-completed-tx.js";
import {
  buildSubmitDepositConfig,
  parseAdditionalAssetsFromRequest,
  parseAddressString,
  parseFundingUtxos,
} from "./submit-deposit.parse-funding-utxos.js";
import { asObject } from "./submit-deposit.reconcile-deposit-submission-attempt-program.js";

export const parseBuildDepositRequest = (
  payload: unknown,
  options?: {
    readonly expectedNetwork?: string;
  },
): BuildDepositRequest => {
  const body = asObject(payload, "Deposit build request");
  const fundingAddress = parseAddressString({
    value: body.fundingAddress,
    field: "fundingAddress",
    expectedNetwork: options?.expectedNetwork,
  });
  const fundingUtxos = parseFundingUtxos({
    value: body.fundingUtxos,
    fundingAddress,
    expectedNetwork: options?.expectedNetwork,
  });

  return {
    ...buildSubmitDepositConfig({
      l2Address: body.l2Address,
      l2Datum: body.l2Datum,
      lovelace: body.lovelace,
      additionalAssets: parseAdditionalAssetsFromRequest(body.additionalAssets),
      expectedNetwork: options?.expectedNetwork,
    }),
    fundingAddress,
    fundingUtxos,
  };
};
