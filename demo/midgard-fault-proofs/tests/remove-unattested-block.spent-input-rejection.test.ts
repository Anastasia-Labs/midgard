/**
 * A CLI timeout-correction run has no recovery reader: an earlier attempt
 * still in flight shows up only as the ledger's refusal of its replacement,
 * which shares its inputs. `isSpentInputSubmitRejection` names exactly the
 * spent-or-unknown-input refusals (the emulator's, Ogmios 3117, the ledger's
 * `BadInputsUTxO`) and nothing else.
 */
import { expect, it } from "vitest";

import { isSpentInputSubmitRejection } from "../src/remove-unattested-block.js";
import { emulatorDoubleSpendRefusal } from "./support/emulator/double-spend-refusal.js";

const OGMIOS_3117 =
  'JSON-RPC error 3117: {"code":3117,"message":"The transaction contains unknown UTxO references as inputs.","data":{"unknownOutputReferences":[{"transaction":{"id":"aa"},"index":0}]}}';

it("classifies the ledger's spent-input refusals, and nothing else, as an in-flight attempt", async () => {
  expect(isSpentInputSubmitRejection(await emulatorDoubleSpendRefusal())).toBe(
    true,
  );
  expect(isSpentInputSubmitRejection(new Error(OGMIOS_3117))).toBe(true);
  expect(
    isSpentInputSubmitRejection(
      new Error("submit failed", {
        cause: { data: { unknownOutputReferences: [] } },
      }),
    ),
  ).toBe(true);
  expect(
    isSpentInputSubmitRejection(
      new Error(
        '{"contents":{"era":"ShelleyBasedEraConway","error":["ConwayUtxowFailure (UtxoFailure (BadInputsUTxO (fromList [])))"]}}',
      ),
    ),
  ).toBe(true);
  for (const other of [
    new Error("socket closed after submission"),
    new Error("OutsideValidityIntervalUTxO"),
    "timeout",
  ])
    expect(isSpentInputSubmitRejection(other)).toBe(false);
});
