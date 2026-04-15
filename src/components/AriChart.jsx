import React, { useRef, useEffect } from 'react';
import * as d3 from 'd3';

export default function AriChart({ data, width = 600, height = 400, margin = { top: 20, right: 60, bottom: 30, left: 0 } }) {
    const svgRef = useRef();
    useEffect(() => {
        if (!data) return;
        const svg = d3.select(svgRef.current);
        svg.selectAll('*').remove();

        // Create chart here
        
    }, [data]);


    return <svg ref={svgRef} width={width} height={height} style={{ overflow: 'visible' }} />;
};
