import React, { useRef, useEffect } from 'react';
import { useMotionValueEvent } from 'motion/react';
import * as d3 from 'd3';
import { annotation, annotationLabel } from 'd3-svg-annotation';

export default function LineChart({ data, xKey, yKey, progress, width = 600, height = 400, margin = { top: 20, right: 60, bottom: 30, left: 0 } }) {
    const svgRef = useRef();
    const pathRefs = useRef({ uchicago: null, ivy: [] });
    const lengthRefs = useRef({ uchicago: 0, ivy: [] });
    const annotationRef = useRef();
    const annotationGroupRef = useRef();
    const textRefs = useRef({ uchicago: null, ivy: [] });
    const labelVisibilityRef = useRef({ annotation: false, uchicago: false, ivy: false });
    useEffect(() => {
        if (!data) return;
        const { uchicagoByYear, ivyPlusByYear } = data;

        const svg = d3.select(svgRef.current);
        svg.selectAll('*').remove();
        const ivySeries = ivyPlusByYear ?? [];
        const ivyPoints = ivySeries.flatMap(d => d.values);
        const allPoints = [...uchicagoByYear, ...ivyPoints];

        const x = d3.scaleLinear()
            .domain(d3.extent(allPoints, d => d[xKey]))
            .range([margin.left, width - margin.right]);

        const y = d3.scaleLinear()
            .domain(d3.extent(allPoints, d => d[yKey])).nice()
            .range([height - margin.bottom, margin.top]);

        const lineInstructions = d3.line()
            .x(d => x(d[xKey]))
            .y(d => y(d[yKey]));


        // Add extra axis-to-data spacing
        const axisPadding = 5;

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
            .attr('font-family', 'Playfair Display, serif')
            .attr('color', 'gray')
            .call(
                d3.axisLeft(y)
                    .tickValues([25, 20, 15, 10, 5, 0])
                    .tickSize(0)
            )
            .call(g => g.select('.domain').remove())
            .call(g => g.selectAll('.tick text')
                .attr('font-size', 16)
                .attr('font-family', 'Playfair Display, serif')
                .attr('color', 'gray')
            );

        // Add horizontal grid lines every 5 units on the y axis
        const yTicks = d3.range(5, 26, 5).reverse();
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



        const ivyInitial = progress ? progress.ivy.get() : 0;
        const uchicagoInitial = progress ? progress.uchicago.get() : 0;

        svg.append('text')
            .attr('x', margin.left)
            .attr('y', margin.top - 25)
            .attr('text-anchor', 'center')
            .attr('fill', 'black')
            .attr('font-size', 22)
            .attr('font-family', 'Georgia, serif')
            .text('% of Economics Majors');

        const ivyPaths = svg.append('g')
            .selectAll('.ivy-plus-line')
            .data(ivySeries)
            .join('path')
            .attr('fill', 'none')
            .attr('class', 'ivy-plus-line')
            .attr('stroke', '#aaa')
            .attr('stroke-linecap', 'round')
            .attr('stroke-width', 1.2)
            .attr('opacity', 0.5)
            .attr('d', d => lineInstructions(d.values));

        pathRefs.current.ivy = ivyPaths.nodes().map(node => d3.select(node));

        pathRefs.current.uchicago = svg.append('path')
            .datum(uchicagoByYear)
            .attr('fill', 'none')
            .attr('class', 'line-path')
            .attr('stroke', '#800000')
            .attr('stroke-width', 4)
            .attr('d', lineInstructions);

        
        lengthRefs.current.uchicago = pathRefs.current.uchicago.node().getTotalLength();
        lengthRefs.current.ivy = pathRefs.current.ivy.map(path => path.node().getTotalLength());

        pathRefs.current.uchicago
            .attr('stroke-dasharray', lengthRefs.current.uchicago)
            .attr('stroke-dashoffset', lengthRefs.current.uchicago * (1 - uchicagoInitial));

        pathRefs.current.ivy.forEach((path, index) => {
            const length = lengthRefs.current.ivy[index];
            path
                .attr('stroke-dasharray', length)
                .attr('stroke-dashoffset', length * (1 - ivyInitial));
        });

        const annotationPoint = uchicagoByYear.find(d => d[xKey] === 2018);   
        const annotations = [
            {
                note: { label: "Business Economics introduced", wrap: 200 }, 
                x: x(annotationPoint[xKey]),
                y: y(annotationPoint[yKey]),
                textAnchor: 'middle',
                dx: -25,
                dy: -35,
                color: "#636363"
            },
        ];

        annotationRef.current = annotation()
            .type(annotationLabel)
            .annotations(annotations);

        annotationGroupRef.current = svg.append('g')
            .attr('class', 'annotation-group')
            .attr('opacity', uchicagoInitial > 0.67 ? 1 : 0)
            .attr('font-size', 17)
            .attr('text-anchor', 'left')
            .attr('font-family', 'Georgia, serif');

        // Add white rect behind annotation (inserted before annotation)
        annotationGroupRef.current.append('rect')
            .attr('x', x(annotationPoint[xKey]) - 110)
            .attr('y', y(annotationPoint[yKey]) - 80)
            .attr('width', 170)
            .attr('height', 40)
            .attr('fill', 'white')
            .attr('opacity', uchicagoInitial > 0.67 ? 1 : 0)

        annotationGroupRef.current.call(annotationRef.current);

    
       

        
        const ivyYs = [208, 222, 236, 251, 265, 280, 297, 314]
        const ivyNames = ["Dartmouth", "Harvard", "Columbia, Yale", "Cornell, Princeton", "Brown, UPenn", "Duke", "Standford", "MIT"]
        const ivyTexts = ivyNames.map((name, i) => {
            return svg.append('text')
                .attr('x', width - margin.right + 5)
                .attr('y', ivyYs[i])
                .attr('text-anchor', 'start')
                .attr('fill', '#666')
                .attr('font-size', 12)
                .attr('opacity', ivyInitial >= 0.2 ? 0.6 : 0)
                .attr('font-family', 'Georgia, serif')
                .text(name);
        });

        const uchicagoText = svg.append('text')
            .attr('x', width - margin.right + 5)
            .attr('y', 65)
            .attr('text-anchor', 'start')
            .attr('fill', '#800000')
            .attr('font-size', 17)
            .attr('opacity', uchicagoInitial > 0.1 ? 1 : 0)
            .attr('font-family', 'Georgia, serif')
            .text('UChicago');

        textRefs.current = { uchicago: uchicagoText, ivy: ivyTexts };
        labelVisibilityRef.current = {
            annotation: uchicagoInitial > 0.67,
            uchicago: uchicagoInitial > 0.1,
            ivy: ivyInitial >= 0.2
        };
    
        
    }, [data]);

    useMotionValueEvent(progress.uchicago, "change", v => {
        if (pathRefs.current.uchicago && lengthRefs.current.uchicago) {
            pathRefs.current.uchicago.attr('stroke-dashoffset', lengthRefs.current.uchicago * (1 - v));
        }
        const showAnnotation = v > 0.67;
        const showUChicago = v >= 0.95;

        if (annotationGroupRef.current && labelVisibilityRef.current.annotation !== showAnnotation) {
            annotationGroupRef.current
              .transition()
              .duration(200)
              .style('opacity', showAnnotation ? 1 : 0);
            labelVisibilityRef.current.annotation = showAnnotation;
        }
        if (textRefs.current.uchicago && labelVisibilityRef.current.uchicago !== showUChicago) {
            textRefs.current.uchicago
                .transition()
                .duration(200)
                .style('opacity', showUChicago ? 1 : 0);
            labelVisibilityRef.current.uchicago = showUChicago;
        }
    });

    useMotionValueEvent(progress.ivy, "change", v => {
        const showIvy = v >= 0.95;

        pathRefs.current.ivy.forEach((path, index) => {
            const length = lengthRefs.current.ivy[index];
            if (path && length) {
                path.attr('stroke-dashoffset', length * (1 - v));
            }
            if (textRefs.current.ivy.length && labelVisibilityRef.current.ivy !== showIvy) {
                textRefs.current.ivy.forEach(text => {
                    text
                        .transition()
                        .duration(200)
                        .style('opacity', showIvy ? 0.6 : 0);
                });
                labelVisibilityRef.current.ivy = showIvy;
            }
        });
    });

    return <svg ref={svgRef} width={width} height={height} style={{ overflow: 'visible' }} />;
};
