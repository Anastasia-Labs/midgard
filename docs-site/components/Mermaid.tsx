'use client';
import { useEffect, useId, useRef, useState } from 'react';

export default function Mermaid({ chart }: { chart: string }) {
  const ref = useRef<HTMLDivElement>(null);
  const [rendered, setRendered] = useState(false);
  const id = useId().replace(/:/g, '');

  useEffect(() => {
    let cancelled = false;
    (async () => {
      const mermaid = (await import('mermaid')).default;
      const root = document.documentElement;
      const dark = root.classList.contains('dark');
      const css = getComputedStyle(root);
      const v = (name: string, fallback: string) =>
        css.getPropertyValue(name).trim() || fallback;

      const l2 = v('--mid-l2', dark ? '#10b981' : '#047857');
      const l1 = v('--mid-l1', dark ? '#5b8dff' : '#1c4fd6');
      const infra = v('--mid-infra', dark ? '#9ca3af' : '#4b5563');
      const surface = dark ? '#111827' : '#ffffff';
      const text = dark ? '#e5e7eb' : '#171717';

      mermaid.initialize({
        startOnLoad: false,
        theme: 'base',
        securityLevel: 'strict',
        fontFamily: 'inherit',
        themeVariables: {
          primaryColor: surface,
          primaryTextColor: text,
          primaryBorderColor: l2,
          lineColor: infra,
          secondaryColor: surface,
          tertiaryColor: surface,
          nodeBorder: l2,
          clusterBorder: l1,
          fontFamily: 'inherit',
        },
      });
      const { svg } = await mermaid.render('mermaid-' + id, chart);
      if (!cancelled && ref.current) {
        ref.current.innerHTML = svg;
        setRendered(true);
      }
    })();
    return () => {
      cancelled = true;
    };
  }, [chart, id]);

  return (
    <div
      ref={ref}
      className="mermaid flex justify-center my-8"
      data-rendered={rendered}
    />
  );
}
