import { Effect } from "effect";
import { vi } from "vitest";

export const fakeWrapperLucid = () => ({
  wallet: () => ({
    address: async () => "addr_test1wrapper",
  }),
  awaitTx: vi.fn<() => Promise<boolean>>(),
});

export const fakeSignBuilder = (signed: unknown) =>
  ({
    toHash: () => "tx-wrapper",
    sign: {
      withWallet: () => ({
        completeProgram: () => Effect.succeed(signed),
      }),
    },
  }) as never;
