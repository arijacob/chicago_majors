import React, { useRef, useEffect } from 'react';
import * as d3 from 'd3';

export default function PennChart({ data, width = 750, height = 400,  margin = { top: 60, right: 80, bottom: 30, left: 50 } }) {
    const svgRef = useRef();

    useEffect(() => {
        if (!data) return;
        const svg = d3.select(svgRef.current);
        svg.selectAll('*').remove();

        // Build two series from the wide-format data
        const uchicagoSeries = data.map(d => ({ year: d.year, value: d.uchicago }));
        const pennSeries = data.map(d => ({ year: d.year, value: d.penn }));
        const allPoints = [...uchicagoSeries, ...pennSeries];

        // Scales
        const x = d3.scaleLinear()
            .domain(d3.extent(allPoints, d => d.year))
            .range([margin.left, width - margin.right]);

        const y = d3.scaleLinear()
            .domain([10, d3.max(allPoints, d => d.value)]).nice()
            .range([height - margin.bottom, margin.top]);

        const lineInstructions = d3.line()
            .x(d => x(d.year))
            .y(d => y(d.value));

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
        const yMax = d3.max(allPoints, d => d.value);
        const yTickStep = yMax > 20 ? 10 : 5;
        const yTicks = d3.range(10, yMax + yTickStep, yTickStep);

        svg.append('g')
            .attr('transform', `translate(${margin.left - axisPadding},0)`)
            .attr('color', 'gray')
            .call(
                d3.axisLeft(y)
                    .tickValues(yTicks)
                    .tickSize(0)
            )
            .call(g => g.select('.domain').remove())
            .call(g => g.selectAll('.tick text')
                .attr('font-size', 16)
                .attr('font-family', 'Playfair Display, serif')
            );

        // Horizontal gridlines
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
            .attr('y', margin.top - 75)
            .attr('fill', 'black')
            .attr('font-size', 22)
            .attr('font-family', 'Georgia, serif')
            .text('Share of Students with Buisness, Economics, and Finance Majors');

        // UChicago line — green
        svg.append('path')
            .datum(uchicagoSeries)
            .attr('fill', 'none')
            .attr('stroke', '#800')
            .attr('stroke-width', 2.5)
            .attr('stroke-linecap', 'round')
            .attr('d', lineInstructions);

        // Penn line — blue
        svg.append('path')
            .datum(pennSeries)
            .attr('fill', 'none')
            .attr('stroke', '#011F5B')
            .attr('stroke-width', 2.5)
            .attr('stroke-linecap', 'round')
            .attr('d', lineInstructions);

        // End-of-line labels
        const lastUchicago = uchicagoSeries[uchicagoSeries.length - 1];
        const lastPenn = pennSeries[pennSeries.length - 1];

        svg.append('text')
            .attr('x', x(2018))
            .attr('y', y(25))
            .attr('dy', '0.35em')
            .attr('fill', '#800')
            .attr('font-size', 18)
            .attr('font-family', 'Georgia, serif')
            .text('UChicago');

        svg.append('text')
            .attr('x', x(2010))
            .attr('y', y(42))
            .attr('dy', '0.35em')
            .attr('fill', '#011F5B')
            .attr('font-size', 18)
            .attr('font-family', 'Georgia, serif')
            .text('University of Pennsylvania');

    }, [data]);

    return <svg ref={svgRef} width={width} height={height} style={{ overflow: 'visible' }} />;
};