import type { ReactNode } from 'react';
import {
  AlertTriangle,
  ShieldCheck,
  User,
  Server,
  Radio,
  Users,
  FileCheck,
} from 'lucide-react';

/** Dangerous / irreversible operations. Stronger than a generic callout. */
export function RiskCallout({
  level = 'high',
  title,
  children,
}: {
  level?: 'critical' | 'high' | 'medium';
  title?: string;
  children: ReactNode;
}) {
  const color = level === 'medium' ? 'var(--mid-pending)' : 'var(--mid-danger)';
  const label =
    level === 'critical' ? 'Critical' : level === 'high' ? 'Danger' : 'Caution';
  return (
    <div
      role="note"
      className="my-6 rounded-lg border-l-4 p-4 bg-fd-card"
      style={{ borderColor: color }}
    >
      <div
        className="flex items-center gap-2 font-display font-semibold"
        style={{ color }}
      >
        <AlertTriangle className="h-4 w-4" aria-hidden />
        <span>{title ?? label}</span>
      </div>
      <div className="mt-2 text-sm text-fd-muted-foreground [&_p]:my-1">
        {children}
      </div>
    </div>
  );
}

/** A protocol rule that must always hold. */
export function InvariantBox({ children }: { children: ReactNode }) {
  return (
    <div
      className="my-6 rounded-lg border p-4 bg-fd-card"
      style={{ borderColor: 'var(--mid-l2)' }}
    >
      <div
        className="flex items-center gap-2 font-display font-semibold"
        style={{ color: 'var(--mid-l2)' }}
      >
        <ShieldCheck className="h-4 w-4" aria-hidden />
        <span>Invariant</span>
      </div>
      <div className="mt-2 text-sm [&_p]:my-1">{children}</div>
    </div>
  );
}

const STATE_META = {
  design: { label: 'Design', color: 'var(--mid-infra)' },
  demo: { label: 'Demo', color: 'var(--mid-l1)' },
  preprod: { label: 'Preprod', color: 'var(--mid-pending)' },
  production: { label: 'Production', color: 'var(--mid-l2)' },
} as const;

/** Inline maturity/status token — always carries a text label (never color alone). */
export function StatePill({ state }: { state: keyof typeof STATE_META }) {
  const m = STATE_META[state] ?? STATE_META.design;
  return (
    <span
      className="inline-flex items-center gap-1.5 rounded-full border px-2 py-0.5 text-xs font-medium font-mono align-middle"
      style={{ color: m.color, borderColor: m.color }}
    >
      <span
        className="h-1.5 w-1.5 rounded-full"
        style={{ background: m.color }}
        aria-hidden
      />
      {m.label}
    </span>
  );
}

const ACTOR_META = {
  user: { icon: User, color: 'var(--mid-l1)', role: 'User' },
  operator: { icon: Server, color: 'var(--mid-l2)', role: 'Operator' },
  watcher: { icon: Radio, color: 'var(--mid-pending)', role: 'Watcher' },
  'da-committee': { icon: Users, color: 'var(--mid-infra)', role: 'DA Committee' },
  validator: { icon: FileCheck, color: 'var(--mid-danger)', role: 'Validator' },
} as const;

/** Describes a protocol actor with consistent iconography. */
export function ActorCard({
  actor,
  name,
  children,
}: {
  actor: keyof typeof ACTOR_META;
  name?: string;
  children?: ReactNode;
}) {
  const m = ACTOR_META[actor] ?? ACTOR_META.user;
  const Icon = m.icon;
  return (
    <div className="my-4 flex gap-3 rounded-lg border border-fd-border p-4 bg-fd-card">
      <span
        className="flex h-9 w-9 shrink-0 items-center justify-center rounded-md"
        style={{
          background: `color-mix(in srgb, ${m.color} 15%, transparent)`,
          color: m.color,
        }}
      >
        <Icon className="h-5 w-5" aria-hidden />
      </span>
      <div>
        <div className="font-display font-semibold text-fd-foreground">
          {name ?? m.role}
        </div>
        <div className="text-xs font-mono" style={{ color: m.color }}>
          {m.role}
        </div>
        <div className="mt-1 text-sm text-fd-muted-foreground [&_p]:my-1">
          {children}
        </div>
      </div>
    </div>
  );
}

const METHOD_COLOR: Record<string, string> = {
  GET: 'var(--mid-l1)',
  POST: 'var(--mid-l2)',
  PUT: 'var(--mid-pending)',
  DELETE: 'var(--mid-danger)',
};

/** Structured HTTP endpoint reference. */
export function EndpointTable({
  rows,
}: {
  rows: { method: string; path: string; auth?: string; description: string }[];
}) {
  return (
    <div className="my-6 overflow-x-auto rounded-lg border border-fd-border">
      <table className="w-full text-sm border-collapse">
        <thead>
          <tr className="bg-fd-muted/50 text-left">
            <th className="p-3 font-medium">Method</th>
            <th className="p-3 font-medium">Path</th>
            <th className="p-3 font-medium">Auth</th>
            <th className="p-3 font-medium">Description</th>
          </tr>
        </thead>
        <tbody>
          {rows.map((r) => (
            <tr
              key={`${r.method}-${r.path}`}
              className="border-t border-fd-border"
            >
              <td
                className="p-3 font-mono font-semibold"
                style={{ color: METHOD_COLOR[r.method] ?? 'var(--mid-infra)' }}
              >
                {r.method}
              </td>
              <td className="p-3 font-mono">{r.path}</td>
              <td className="p-3 text-fd-muted-foreground">{r.auth ?? '—'}</td>
              <td className="p-3 text-fd-muted-foreground">{r.description}</td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}
