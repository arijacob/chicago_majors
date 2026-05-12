import React, { useRef, useEffect } from 'react';
import { useMotionValueEvent } from 'motion/react';
import * as d3 from 'd3';

export default function LineChart({ data, xKey, yKey, progress, width = 600, height = 400, margin = { top: 20, right: 60, bottom: 30, left: 0 } }) {
    const svgRef = useRef();
    const pathRefs = useRef({ uchicago: null, uchicagoInner: null, ivy: [], hum: null });
    const lengthRefs = useRef({ uchicago: 0, ivy: [], hum: 0});
    const textRefs = useRef({ uchicago: null, ivy: [] });
    const labelVisibilityRef = useRef({ uchicago: false, ivy: false });
    const annotationDottedLineRef = useRef();
    const annotationLabelRef = useRef();
    const titleRef = useRef();

    useEffect(() => {
        if (!data) return;
        const { uchicagoByYear, ivyPlusByYear, humByYear } = data;

        const svg = d3.select(svgRef.current);
        svg.selectAll('*').remove();

        const ivySeries = ivyPlusByYear ?? [];
        const ivyPoints = ivySeries.flatMap(d => d.values);
        const allPoints = [...uchicagoByYear, ...ivyPoints];

        // Scales
        const x = d3.scaleLinear()
            .domain(d3.extent(allPoints, d => d[xKey]))
            .range([margin.left, width - margin.right]);

        const y = d3.scaleLinear()
            .domain(d3.extent(allPoints, d => d[yKey])).nice()
            .range([height - margin.bottom, margin.top]);

        const lineInstructions = d3.line()
            .x(d => x(d[xKey]))
            .y(d => y(d[yKey]));

        const axisPadding = 5;

        // x axis
        svg.append('g')
            .attr('transform', `translate(0,${height - margin.bottom + axisPadding})`)
            .attr('color', 'gray')
            .call(
                d3.axisBottom(x)
                    .tickFormat(d => d.toString())
                    .tickSizeOuter(0)
            )
            .call(g => g.selectAll('.tick text')
                .attr('font-size', 16)
                .attr('font-family', 'Playfair Display, serif')
            );

        // y axis
        svg.append('g')
            .attr('transform', `translate(${margin.left - axisPadding},0)`)
            .attr('color', 'gray')
            .call(
                d3.axisLeft(y)
                    .tickValues([40, 30, 20, 10, 0])
                    .tickFormat(d => `${d}%`)
                    .tickSize(0)
            )
            .call(g => g.select('.domain').remove())
            .call(g => g.selectAll('.tick text')
                .attr('font-size', 16)
                .attr('font-family', 'Playfair Display, serif')
            );

        // Horizontal gridlines
        const yTicks = d3.range(0, 41, 10).reverse();
        svg.append('g')
            .attr('class', 'y-grid')
            .selectAll('line')
            .data(yTicks)
            .join('line')
            .attr('x1', margin.left)
            .attr('x2', width - margin.right)
            .attr('y1', d => y(d))
            .attr('y2', d => y(d))
            .attr('stroke', '#e6e6e6')
            .attr('stroke-width', 1);

        // Read initial progress values
        const uchicagoInitial = progress ? progress.uchicago.get() : 0;
        const ivyInitial = progress ? progress.ivy.get() : 0;
        const annotationInitial = progress ? progress.annotation.get() : 0;

        // Chart title
        titleRef.current = svg.append('text')
            .attr('x', margin.left)
            .attr('y', margin.top - 15)
            .attr('text-anchor', 'start')
            .attr('fill', 'black')
            .attr('font-size', 22)
            .attr('font-family', 'Georgia, serif')
            .text('Share of Students Majoring in Economics');

        // Dotted vertical line at 2018 (initially hidden)
        const annotationYear = 2018;

        annotationDottedLineRef.current = svg.append('line')
            .attr('x1', x(annotationYear))
            .attr('x2', x(annotationYear))
            .attr('y1', margin.top)
            .attr('y2', height - margin.bottom)
            .attr('stroke', '#666')
            .attr('stroke-width', 1)
            .attr('stroke-dasharray', '4,4')
            .attr('opacity', annotationInitial);

        annotationLabelRef.current = svg.append('text')
            .attr('x', x(2013.5))
            .attr('y', margin.top + 100)
            .attr('text-anchor', 'middle')
            .attr('fill', '#666')
            .attr('font-size', 16)
            .attr('font-family', 'Georgia, serif')
            .attr('opacity', annotationInitial)
            .text('Business Economics introduced');

        // Ivy Plus lines
        const ivyPaths = svg.append('g')
            .selectAll('.ivy-plus-line')
            .data(ivySeries)
            .join('path')
            .attr('fill', 'none')
            .attr('class', 'ivy-plus-line')
            .attr('stroke', '#1e1e1e')
            .attr('stroke-linecap', 'round')
            .attr('stroke-width', 1.2)
            .attr('opacity', 0.3)
            .attr('d', d => lineInstructions(d.values));

        pathRefs.current.ivy = ivyPaths.nodes().map(node => d3.select(node));

        // UChicago line + thicker inner highlight
        pathRefs.current.uchicago = svg.append('path')
            .datum(uchicagoByYear)
            .attr('fill', 'none')
            .attr('class', 'line-path')
            .attr('stroke', '#800000')
            .attr('stroke-width', 2)
            .attr('d', lineInstructions);

        pathRefs.current.uchicagoInner = svg.append('path')
            .datum(uchicagoByYear)
            .attr('fill', 'none')
            .attr('class', 'line-path-inner')
            .attr('stroke', '#800000')
            .attr('stroke-width', 3)
            .attr('d', lineInstructions);

        // Humanities line — forest green, initially hidden
        const humInitial = progress ? progress.hum.get() : 0;

        pathRefs.current.hum = svg.append('path')
            .datum(humByYear)
            .attr('fill', 'none')
            .attr('class', 'line-path-hum')
            .attr('stroke', '#2d5a3f')
            .attr('stroke-width', 3)
            .attr('d', lineInstructions);

        lengthRefs.current.hum = pathRefs.current.hum.node().getTotalLength();

        pathRefs.current.hum
            .attr('stroke-dasharray', lengthRefs.current.hum)
            .attr('stroke-dashoffset', lengthRefs.current.hum * (1 - humInitial));

        // Measure path lengths for the draw-on animation
        lengthRefs.current.uchicago = pathRefs.current.uchicago.node().getTotalLength();
        lengthRefs.current.ivy = pathRefs.current.ivy.map(path => path.node().getTotalLength());

        pathRefs.current.uchicago
            .attr('stroke-dasharray', lengthRefs.current.uchicago)
            .attr('stroke-dashoffset', lengthRefs.current.uchicago * (1 - uchicagoInitial));

        pathRefs.current.uchicagoInner
            .attr('stroke-dasharray', lengthRefs.current.uchicago)
            .attr('stroke-dashoffset', lengthRefs.current.uchicago * (1 - uchicagoInitial));

        pathRefs.current.ivy.forEach((path, index) => {
            const length = lengthRefs.current.ivy[index];
            path
                .attr('stroke-dasharray', length)
                .attr('stroke-dashoffset', length * (1 - ivyInitial));
        });

        // School name labels (right side)
        const ivyYs = [208, 222, 236, 251, 265, 280, 297, 314];
        const ivyNames = ["Dartmouth", "Harvard", "Columbia, Yale", "Cornell, Princeton", "Brown, UPenn", "Duke", "Stanford", "MIT"];
        const ivyTexts = ivyNames.map((name, i) => {
            return svg.append('text')
                .attr('x', width - margin.right - 5)
                .attr('y', ivyYs[i])
                .attr('text-anchor', 'start')
                .attr('fill', '#1e1e1e')
                .attr('font-size', 12)
                .attr('opacity', ivyInitial >= 0.2 ? 0.6 : 0)
                .attr('font-family', 'Georgia, serif')
                .text(name);
        });

        const uchicagoText = svg.append('text')
            .attr('x', width - margin.right - 100)
            .attr('y', 55)
            .attr('text-anchor', 'start')
            .attr('fill', '#800000')
            .attr('font-size', 17)
            .attr('opacity', uchicagoInitial > 0.1 ? 1 : 0)
            .attr('font-family', 'Georgia, serif')
            .text('UChicago');

        const humLabel = svg.append('text')
            .attr('x', x(2007))
            .attr('y', y(35))
            .attr('dy', '0.35em')
            .attr('text-anchor', 'start')
            .attr('fill', '#2d5a3f')
            .attr('font-size', 17)
            .attr('opacity', humInitial > 0.95 ? 1 : 0)
            .attr('font-family', 'Georgia, serif')
            .text('Humanities and Arts');

        const econLabel = svg.append('text')
            .attr('x', x(2018.25))
            .attr('y', y(38))
            .attr('dy', '0.35em')
            .attr('text-anchor', 'start')
            .attr('fill', '#800000')
            .attr('font-size', 17)
            .attr('opacity', humInitial)
            .attr('font-family', 'Georgia, serif')
            .text('Economics');

        textRefs.current = { uchicago: uchicagoText, ivy: ivyTexts, hum: humLabel, econ: econLabel};
        labelVisibilityRef.current = {
            uchicago: uchicagoInitial > 0.1,
            ivy: ivyInitial >= 0.2
        };
    }, [data]);

    // UChicago line draw-on + label fade-in
    useMotionValueEvent(progress.uchicago, "change", v => {
        if (pathRefs.current.uchicago && lengthRefs.current.uchicago) {
            pathRefs.current.uchicago.attr('stroke-dashoffset', lengthRefs.current.uchicago * (1 - v));
        }
        if (pathRefs.current.uchicagoInner && lengthRefs.current.uchicago) {
            pathRefs.current.uchicagoInner.attr('stroke-dashoffset', lengthRefs.current.uchicago * (1 - v));
        }
        const showUChicago = v >= 0.95;
        if (textRefs.current.uchicago && labelVisibilityRef.current.uchicago !== showUChicago) {
            textRefs.current.uchicago.attr('opacity', showUChicago ? 1 : 0);
            labelVisibilityRef.current.uchicago = showUChicago;
        }
    });

    // Ivy Plus draw-on + labels fade-in
    useMotionValueEvent(progress.ivy, "change", v => {
        pathRefs.current.ivy.forEach((path, index) => {
            const length = lengthRefs.current.ivy[index];
            if (path && length) {
                path.attr('stroke-dashoffset', length * (1 - v));
            }
        });

        const showIvy = v >= 0.95;
        if (textRefs.current.ivy.length && labelVisibilityRef.current.ivy !== showIvy) {
            textRefs.current.ivy.forEach(text => {
                text.attr('opacity', showIvy ? 0.6 : 0);
            });
            labelVisibilityRef.current.ivy = showIvy;
        }
    });

    // Ivy Plus fade-out (lines and labels)
    useMotionValueEvent(progress.ivyFade, "change", v => {
        pathRefs.current.ivy.forEach(path => {
            if (path) {
                path.attr('opacity', 0.3 * (1 - v));
            }
        });
        textRefs.current.ivy.forEach(text => {
            if (text) {
                text.attr('opacity', 0.6 * (1 - v));
            }
        });
    });

    // "Business Economics introduced" annotation fade-in
    useMotionValueEvent(progress.annotation, "change", v => {
        if (annotationDottedLineRef.current) {
            annotationDottedLineRef.current.attr('opacity', v);
        }
        if (annotationLabelRef.current) {
            annotationLabelRef.current.attr('opacity', v);
        }
    });

    // "Business Economics introduced" annotation fade-out
    useMotionValueEvent(progress.annotationFade, "change", v => {
        if (annotationDottedLineRef.current) {
            annotationDottedLineRef.current.attr('opacity', 1 - v);
        }
        if (annotationLabelRef.current) {
            annotationLabelRef.current.attr('opacity', 1 - v);
        }
        if (textRefs.current.uchicago) {
            textRefs.current.uchicago.attr('opacity', 1 - v);
        }
    });

    useMotionValueEvent(progress.hum, "change", v => {
        // Draw the humanities line
        if (pathRefs.current.hum && lengthRefs.current.hum) {
            pathRefs.current.hum.attr('stroke-dashoffset', lengthRefs.current.hum * (1 - v));
        }

        // Fade in the humanities label
        if (textRefs.current.hum) {
            textRefs.current.hum.attr('opacity', v);
        }

        // Fade in econ label
        if (textRefs.current.econ) {
            textRefs.current.econ.attr('opacity', v);
        }

         // Swap the title once we cross the threshold
        if (titleRef.current) {
            const newTitle = v > 0.05
                ? 'Student Majors at UChicago'
                : 'Share of Students Majoring in Economics';
            titleRef.current.text(newTitle);
        }
    });

    return <svg ref={svgRef} width={width} height={height} style={{ overflow: 'visible' }} />;
};