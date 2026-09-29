/**
 * Walking an error's cause chain, so a failure record carries the node's
 * refusal reason and not only the outermost wrapper's message. Kept free of
 * the SDK and Lucid so the stage timing and the journey core can use it.
 */
import { inspect } from "node:util";

import { Cause, Runtime } from "effect";

/**
 * Every link of an error chain, outermost first: the error itself, the
 * failures and defects of an Effect `FiberFailure`, `AggregateError` members
 * and `cause` links. Each object appears once.
 */
export const errorChainLinks = (error: unknown): readonly unknown[] => {
  const seen = new Set<unknown>();
  const links: unknown[] = [];
  const visit = (value: unknown): void => {
    if (value === null || value === undefined || seen.has(value)) return;
    links.push(value);
    if (typeof value !== "object") return;
    seen.add(value);
    if (Runtime.isFiberFailure(value)) {
      const cause = value[Runtime.FiberFailureCauseId];
      for (const failure of [...Cause.failures(cause), ...Cause.defects(cause)])
        visit(failure);
    }
    if (value instanceof AggregateError)
      for (const member of value.errors) visit(member);
    if ("cause" in value) visit((value as { cause?: unknown }).cause);
  };
  visit(error);
  return links;
};

/** `data` as JSON; never throws, so a BigInt cannot replace the real failure. */
const dataText = (data: unknown): string => {
  try {
    return (
      JSON.stringify(data, (_key, value: unknown) =>
        typeof value === "bigint" ? value.toString() : value,
      ) ?? ""
    );
  } catch {
    return inspect(data);
  }
};

/**
 * The text of every link in an error chain (see `errorChainLinks`). Each link
 * gives its message and, when it has one, its `data` as JSON (where Ogmios
 * puts a refusal's details).
 */
export const errorChainTexts = (error: unknown): readonly string[] =>
  errorChainLinks(error).flatMap((link): string[] => {
    if (typeof link !== "object" || link === null) return [String(link)];
    const fields = link as {
      readonly message?: unknown;
      readonly data?: unknown;
    };
    const message = typeof fields.message === "string" ? fields.message : "";
    const data = "data" in fields ? dataText(fields.data) : "";
    if (message.length === 0 && data === "") return [];
    return [data === "" ? message : `${message} ${data}`];
  });

/** The whole chain on one line, outermost first. */
export const describeErrorChain = (error: unknown): string =>
  errorChainTexts(error).join(" <- ");
