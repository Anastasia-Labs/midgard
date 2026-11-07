// W4.2: `localeCompare` must name its locale.
//
// Owner ruling (2026-09-24): canonical JSON orders object keys with the
// collation pinned to "en" (`compareCanonicalJsonKeys` in
// midgard-core/src/canonical-json.ts). A bare `a.localeCompare(b)` follows the
// host's default locale, so the same record can sort, and therefore hash,
// differently on two machines. The watcher's code-unit ordering is a
// deliberate, different choice and is not a `localeCompare` call; this rule
// does not touch it.

import { defineRule } from "../baseline.mjs";
import { calleeName, unwrap } from "../ast.mjs";

export default defineRule({
  meta: {
    type: "problem",
    docs: {
      description:
        "Require an explicit locale argument to String.prototype.localeCompare.",
    },
    schema: [],
    messages: {
      bareLocaleCompare:
        '`localeCompare` without a locale follows the host\'s default collation, so the same strings can sort differently on two machines. Fix: pass the locale (`a.localeCompare(b, "en")`); order the keys of a hashed record with `compareCanonicalJsonKeys` from `@al-ft/midgard-core/canonical-json`; compare with `<`/`>` when you want code-unit order. If this order feeds a digest, a journal or an on-chain commitment, changing its collation is a consensus change: ask the owner first.',
    },
  },
  create: (_context, report) => ({
    CallExpression(node) {
      const callee = unwrap(node.callee);
      if (callee.type !== "MemberExpression") return;
      if (calleeName(node) !== "localeCompare") return;
      const locale = node.arguments[1];
      const missing =
        locale === undefined ||
        (unwrap(locale).type === "Identifier" &&
          unwrap(locale).name === "undefined");
      if (missing) {
        report({ node: callee.property, messageId: "bareLocaleCompare" });
      }
    },
  }),
});
