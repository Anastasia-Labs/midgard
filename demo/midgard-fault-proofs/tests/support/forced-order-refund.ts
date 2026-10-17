import type { LucidEvolution } from "@lucid-evolution/lucid";

/** Rewrites the CBOR of an order datum before the mint pays it. */
export type OrderDatumRewrite = (datumCbor: string) => string;

/**
 * `lucid` whose transaction builders pay any inline datum at `orderAddress`
 * through `rewrite`, as a builder other than the SDK's would write it. The
 * SDK's builder is otherwise unchanged, so the order still carries the SDK's
 * own id, inclusion time, witness and payload.
 */
export const withOrderDatumRewrite = (
  lucid: LucidEvolution,
  orderAddress: string,
  rewrite: OrderDatumRewrite,
): LucidEvolution => {
  const intercept = (builder: object): object => {
    const wrap = (target: object): object =>
      new Proxy(target, {
        get(on, property) {
          const value: unknown = Reflect.get(on, property, on);
          if (typeof value === "function")
            return (...args: unknown[]) => {
              const [address, datum, ...rest] = args;
              const inline = datum as { kind?: unknown; value?: unknown };
              const result: unknown =
                property === "ToAddressWithData" &&
                address === orderAddress &&
                inline.kind === "inline" &&
                typeof inline.value === "string"
                  ? value.apply(on, [
                      address,
                      { ...inline, value: rewrite(inline.value) },
                      ...rest,
                    ])
                  : value.apply(on, args);
              return result === builder ? proxy : result;
            };
          return value !== null && typeof value === "object"
            ? wrap(value)
            : value;
        },
      });
    const proxy = wrap(builder);
    return proxy;
  };
  return new Proxy(lucid, {
    get(on, property) {
      const value: unknown = Reflect.get(on, property, on);
      if (property === "newTx")
        return () => intercept(on.newTx()) as ReturnType<typeof on.newTx>;
      return typeof value === "function" ? value.bind(on) : value;
    },
  });
};
