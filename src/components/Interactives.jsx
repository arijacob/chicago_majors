import React, { useRef, useEffect, useState } from 'react';
import { motion, useScroll, useTransform } from "motion/react"
import { Scrollama, Step } from 'react-scrollama';
import * as d3 from 'd3';
import LineChart from './LineChart';


const ScrollBar = ({ scrollYProgress }) => {
    return (
        <div className="absolute left-0 right-0 z-10 flex justify-center">
            <div className="relative w-full h-[8px]
             overflow-hidden
            lg:max-w-none lg:h-[8px]">
                <motion.div
                    className="absolute
                    inset-0 rounded-full bg-[#800000] origin-left"
                    style={{ scaleX: scrollYProgress, transformOrigin: "left" }}
                />
            </div>
        </div>
    )
}




export const ScrollContainer = (props) => {
    const { start, onStepEnter, onStepExit, textArray, height } = props;
    return (
        <div className="relative px-5 py-5 z-10 mx-auto">
            <Scrollama
                onStepEnter={onStepEnter}
                onStepExit={onStepExit}
                offset={0.5}
            >
                {textArray.map((_, index) => (
                    <Step data={start + index} key={start + index}>
                        <div
                            className="relative w-[100px] h-[100px]"
                            style={{ marginBottom: 0.45 * height + 'px' }}
                        >
                            <p
                                className="scroll_font text-center"
                            ></p>
                        </div>
                    </Step>
                ))}
            </Scrollama>
        </div>
    );
};

const AnimationContainerTwo = (props) => {
    const { currentStepIndex, textArray,
        scrollYProgress, barStart,barLength, imageArray, height, size } = props;


    const barProgress = useTransform(
        scrollYProgress,
        [barStart, barLength],
        [0, 1]
    )


    return (
        <div style={{ height: `${size * textArray.length * height}px` }}>
            <div className="sticky top-0 h-screen w-full relative flex justify-center">
                <ScrollBar scrollYProgress={barProgress} />
            {imageArray.map((el, index) => (
                <div key={index} className={`absolute top-0 left-1/2 -translate-x-1/2
                max-w-xl w-full max-h-screen
                lg:flex lg:items-center lg:justify-center lg:h-screen
                lg:p-6 lg:gap-10 lg:max-w-6xl lg:w-full
                ${((el[1] <= currentStepIndex && currentStepIndex <= el[2]) || currentStepIndex == 11) ? 'z-30 pointer-events-auto' : 'z-0 pointer-events-none'}`}>
                    <div className="flex flex-col items-center lg:flex-[2] lg:min-w-0 pointer-events-auto">
                        <img
                            src={el[0]}
                            className={`mt-[20px] top-0 w-full h-auto object-contain lg:mt-0 lg:max-w-5xl lg:w-full
                                transition-opacity duration-[1500ms] max-h-[70vh]
                                ${el[1] <= currentStepIndex && currentStepIndex <= el[2] || 
                                    (currentStepIndex == 11  && el[1] == 0) ? 'opacity-100' : 'opacity-0'}`}
                        />
                        <p
                            className={`mt-[10px] lg:mt-[20px] top-0 w-full text-sm px-5 lg:px-0
                            ${el[1] <= currentStepIndex && currentStepIndex <= el[2] || 
                                (currentStepIndex == 11  && el[1] == 0) ? 'opacity-100' : 'opacity-0'}`}
                            dangerouslySetInnerHTML={{ __html: el[3] }}
                        ></p>
                        
                    </div>
                    <div className="z-[25] absolute left-0 right-0
                        flex justify-center top-[105%] sm:top-[110%]
                        lg:relative lg:top-0 lg:left-0 lg:right-0 lg:flex-1
                        lg:min-w-0 lg:flex lg:items-center lg:justify-center">
                        <p
                        className={`absolute caption lg:relative lg:text-left p-5
                        ${el[1] <= currentStepIndex && currentStepIndex <= el[2] || 
                            (currentStepIndex == 11  && el[1] == 0) ? 'opacity-100' : 'opacity-0'}`}
                        dangerouslySetInnerHTML={{ __html: textArray[index] }}
                        />
                    </div>
                </div>
        ))}
            </div>
        </div>
    );
};

export const AnimationBoxTwo = (props) => {
    const {
        currentStepIndex,
        scrollText,
        imageArray,
        barStart = 0,
        barLength = 1,
        size = 1.2,
        onStepEnter,
        onStepExit,
        height,
        width,
        start,
    } = props;
    const stepsContainerRef = useRef(null);
    const { scrollYProgress } = useScroll({
        target: stepsContainerRef,
        offset: ["start end", "end start"],
    });

    return (
        <div className="relative">
            <AnimationContainerTwo
                currentStepIndex={currentStepIndex}
                textArray={scrollText}
                scrollYProgress={scrollYProgress}
                imageArray={imageArray}
                height={height}
                width={width}
                barLength={barLength}
                barStart={barStart}
                size={size}
            />
            <div ref={stepsContainerRef} className="absolute top-[50vh]">
                <ScrollContainer
                    onStepEnter={onStepEnter}
                    onStepExit={onStepExit}
                    textArray={scrollText}
                    start={start}
                    height={height}
                />
            </div>
        </div>
    );
}

export const IntroAnimation = (props) => {

    const [chartData, setChartData] = useState(null);
    const containerRef = useRef(null);
    
    const { scrollYProgress } = useScroll({
        target: containerRef,
        offset: ["start end", "end start"],
    });

    const uchicagoProgress = useTransform(
        scrollYProgress,
        [0.16, 0.335],
        [0, 1]
    );

    const ivyProgress = useTransform(
        scrollYProgress,
        [0.335, 0.85],
        [0, 1]
    );

    const progresses = {uchicago: uchicagoProgress, ivy: ivyProgress};
    console.log(uchicagoProgress.current)

    useEffect(() => {
        Promise.all([
            d3.csv("data/majors.csv", d3.autoType),
            d3.csv("data/classifications.csv", d3.autoType)
        ]).then(([majors, classifications]) => {
            const econBusiness = majors.filter(d =>
                d.cip_code.includes("Economics") || d.cip_code.includes("Business")
            );

            const uchicagoByYear = d3.rollups(
                    econBusiness.filter(d => d.instnm === "University of Chicago"),
                    v => ({
                        total: d3.sum(v, d => d.total),
                        degrees: v[0].total_degrees
                    }),
                    d => d.year
                )
                .map(([year, vals]) => ({ 
                    year, 
                    total: (vals.total / vals.degrees) * 100
                }))
                .sort((a, b) => a.year - b.year);

            const ivyPlusByYear = d3.rollups(
                    econBusiness.filter(d => d.instnm != "University of Chicago"),
                    v => ({
                        total: d3.sum(v, d => d.total),
                        degrees: v[0].total_degrees
                    }),
                    d => d.instnm,
                    d => d.year
                )
                .map(([instnm, yearlyValues]) => ({
                    instnm,
                    values: yearlyValues
                        .map(([year, vals]) => ({
                            year,
                            total: (vals.total / vals.degrees) * 100
                        }))
                        .sort((a, b) => a.year - b.year)
                }))
                .sort((a, b) => d3.ascending(a.instnm, b.instnm));

            // Log an ordered list of totals in 2024 for the ivyPlusByYear
            const ivyTotals2024 = ivyPlusByYear
                .map(instObj => {
                    const year2024 = instObj.values.find(v => v.year === 2024);
                    return {
                        instnm: instObj.instnm,
                        total: year2024 ? year2024.total : null
                    };
                })
                .filter(d => d.total !== null)
                .sort((a, b) => b.total - a.total); // e.g. descending order; adjust as needed

            console.log("IvyPlus 2024 totals ordered:", ivyTotals2024);

            const codes = classifications
                .map(d => ({ ...d }))
                .sort((a, b) => d3.ascending(a.cip_code, b.cip_code));

            setChartData({uchicagoByYear: uchicagoByYear, ivyPlusByYear: ivyPlusByYear});
        });

    }, []);

    return (
        <div ref={containerRef} style={{ height: '600vh' }}>
            <div className="sticky top-0 h-screen w-full flex justify-center items-center">
                {chartData && <LineChart data={chartData} xKey="year" yKey="total" progress={progresses} />}
            </div>
        </div>
    )
}