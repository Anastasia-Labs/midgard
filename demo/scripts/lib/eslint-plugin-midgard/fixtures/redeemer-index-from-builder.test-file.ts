// fixture-path: midgard-fault-proofs/tests/redeemer-wire.test.ts
declare const lucid: any;
declare const Data: any;
declare const it: (name: string, body: () => unknown) => void;
declare const expect: (value: unknown) => { toBe: (expected: unknown) => void };
declare const Step01SpendRedeemer: unknown;
declare const thread: unknown;
declare const faultProofFieldOpening: (options: object) => unknown;
declare const planned: unknown;
declare const carriageUtxos: readonly unknown[];

// A wire vector that builds no transaction: the literal is an encoding input,
// not a position in any ledger order.
it("encodes the step-01 redeemer", () => {
  const bytes = Data.to(
    {
      Continue: [
        {
          // ok: midgard/redeemer-index-from-builder
          input_index: 0n,
        },
      ],
    },
    Step01SpendRedeemer,
  );
  expect(bytes.length > 0).toBe(true);
  const opening = faultProofFieldOpening({
    planned,
    // ok: midgard/redeemer-index-from-builder
    referenceInputs: carriageUtxos,
  });
  expect(opening).toBe(opening);
});

// A test helper that does build a transaction is held to the same rule as
// production code.
const submitRaw = () =>
  lucid.newTx().collectFrom(
    [thread],
    Data.to(
      {
        Continue: [
          {
            // ruleid: midgard/redeemer-index-from-builder
            input_index: 0n,
          },
        ],
      },
      Step01SpendRedeemer,
    ),
  );

it("submits", () => submitRaw());
