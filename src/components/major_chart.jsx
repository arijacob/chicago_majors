import React, { useRef, useEffect } from 'react';
import * as d3 from 'd3';

const COLORS = ['#800', '#1e3a5f', '#2a7f3e', '#c45a00', '#5e35b1', '#00838f', '#d81b60', '#5d4037'];

export default function MajorChart({ series, width = 700, height = 400, margin = { top: 60, right: 50, bottom: 30, left: 50 } }) {
    const svgRef = useRef();

    useEffect(() => {
        if (!series || series.length === 0) return;
        const svg = d3.select(svgRef.current);
        svg.selectAll('*').remove();

        // Combine all points to compute scale domains
        const allPoints = series.flatMap(s => s.values);
        if (allPoints.length === 0) return;

        // Scales
        const x = d3.scaleLinear()
            .domain(d3.extent(allPoints, d => d.year))
            .range([margin.left, width - margin.right]);

        const yMax = d3.max(allPoints, d => d.value);
        const niceSteps = [0.1, 0.25, 0.5, 1, 2, 2.5, 5, 10, 20, 25];
        const targetTickCount = 5;
        const idealStep = yMax / targetTickCount;
        const yTickStep = niceSteps.find(s => s >= idealStep) || niceSteps[niceSteps.length - 1];
        const domainTop = Math.ceil(yMax / yTickStep) * yTickStep;
        const yTicks = d3.range(0, domainTop + yTickStep / 2, yTickStep);

        const y = d3.scaleLinear()
            .domain([0, domainTop])
            .range([height - margin.bottom, margin.top]);

        const lineInstructions = d3.line()
            .x(d => x(d.year))
            .y(d => y(d.value));

        const axisPadding = 5;

        // x axis
        svg.append('g')
            .attr('transform', `translate(0,${height - margin.bottom + axisPadding})`)
            .attr('color', 'gray')
            .call(d3.axisBottom(x).tickFormat(d => d.toString()).tickSizeOuter(0))
            .call(g => g.selectAll('.tick text')
                .attr('font-size', 16)
                .attr('font-family', 'Playfair Display, serif')
            );

        // y axis
        svg.append('g')
            .attr('transform', `translate(${margin.left - axisPadding},0)`)
            .attr('color', 'gray')
            .call(d3.axisLeft(y)
                .tickValues(yTicks)
                .tickFormat(d => {
                    if (yTickStep >= 1) return `${d}%`;
                    if (yTickStep >= 0.5) return `${d.toFixed(1)}%`;
                    return `${d.toFixed(2)}%`;
                })
                .tickSize(0)
            )
            .call(g => g.select('.domain').remove())
            .call(g => g.selectAll('.tick text')
                .attr('font-size', 16)
                .attr('font-family', 'Playfair Display, serif')
            );

        // Gridlines
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

        // Chart title
        svg.append('text')
            .attr('x', margin.left)
            .attr('y', margin.top - 25)
            .attr('fill', 'black')
            .attr('font-size', 22)
            .attr('font-family', 'Georgia, serif')
            .text('Share of UChicago Majors');

        // Draw one line per series
        series.forEach((s, i) => {
            const color = COLORS[i % COLORS.length];

            svg.append('path')
                .datum(s.values)
                .attr('fill', 'none')
                .attr('stroke', color)
                .attr('stroke-width', 2.5)
                .attr('stroke-linecap', 'round')
                .attr('d', lineInstructions);

            // Endpoint label
            const lastPoint = s.values[s.values.length - 1];
            if (lastPoint) {
                svg.append('text')
                    .attr('x', x(lastPoint.year))
                    .attr('y', y(lastPoint.value))
                    .attr('dx', 6)
                    .attr('dy', '0.35em')
                    .attr('fill', color)
                    .attr('font-size', 14)
                    .attr('font-family', 'Georgia, serif')
                    .text(s.major);
            }
        });

    }, [series]);

    return (
        <svg
            ref={svgRef}
            viewBox={`0 0 ${width} ${height}`}
            preserveAspectRatio="xMidYMid meet"
            style={{ width: '100%', height: 'auto', display: 'block', overflow: 'visible' }}
        />
    );
    // return <svg ref={svgRef} width={width} height={height} style={{ overflow: 'visible' }} />;
}