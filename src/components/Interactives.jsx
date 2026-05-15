import React, { useRef, useEffect, useState } from 'react';
import { motion, useScroll, useTransform } from "motion/react"
import { Scrollama, Step } from 'react-scrollama';
import * as d3 from 'd3';
import LineChart from './LineChart';
import AriChart from './english_polisci_chart';
import SubstitutionChart from './substitution_chart';
import PennChart from './penn_chart';
import OutcomesChart from './outcomes_chart';
import SocialHumChart from './socialhum_chart';
import MajorChart from './major_chart';

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
};


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

const TextStep = ({ v, scrollY, children }) => {
    const [start, end] = v;
    const opacity = useTransform(scrollY, [start - 0.03, start, end, end + 0.03], [0, 1, 1, 0]);
    return (
        <div className="min-h-[150vh] flex items-center">
            <motion.div
                style={{ opacity }}
                className="sticky top-[75vh] lg:top-[50vh] px-6 lg:px-8"
            >
                {children}
            </motion.div>
        </div>
    );
};

export const IntroAnimation = (props) => {

    const [chartData, setChartData] = useState(null);
    const containerRef = useRef(null);
    
    const { scrollYProgress } = useScroll({
        target: containerRef,
        offset: ["start end", "end start"],
    });

    const uchicagoProgress = useTransform(scrollYProgress, [0.12, 0.28], [0, 1]);
    const ivyProgress = useTransform(scrollYProgress, [0.32, 0.46], [0, 1]);
    const ivyFadeProgress = useTransform(scrollYProgress, [0.50, 0.52], [0, 1]);
    const annotationProgress = useTransform(scrollYProgress, [0.52, 0.55], [0, 1]);
    const annotationFade = useTransform(scrollYProgress, [0.62, 0.68], [0, 1]);
    const humProgress = useTransform(scrollYProgress, [0.68, 0.82], [0, 1]);


    const progresses = {
        uchicago: uchicagoProgress,
         ivy: ivyProgress,
          ivyFade: ivyFadeProgress,
           annotation: annotationProgress,
           annotationFade: annotationFade,
            hum: humProgress,
        };
    console.log(uchicagoProgress.current)

    useEffect(() => {
        Promise.all([
            d3.csv("data/majors.csv", d3.autoType),
            d3.csv("data/classifications.csv", d3.autoType),
            d3.csv("data/economics_humanities.csv", d3.autoType)
        ]).then(([majors, classifications, humanities]) => {
            const econBusiness = majors.filter(d =>
                d.cip_code.includes("Economics") || d.cip_code.includes("Business")
            );

            const uchicagoByYear = d3.rollups(
                    econBusiness.filter(d => d.instnm === "University of Chicago"),
                    v => ({
                        total: d3.sum(v, d => d.total),
                        students: v[0].total_students
                    }),
                    d => d.year
                )
                .map(([year, vals]) => ({ 
                    year, 
                    total: (vals.total / vals.students) * 100
                }))
                .concat({ year: 2025, total: 41 })
                .sort((a, b) => a.year - b.year);

            const ivyPlusByYear = d3.rollups(
                    econBusiness.filter(d => d.instnm != "University of Chicago"),
                    v => ({
                        total: d3.sum(v, d => d.total),
                        students: v[0].total_students
                    }),
                    d => d.instnm,
                    d => d.year
                )
                .map(([instnm, yearlyValues]) => ({
                    instnm,
                    values: yearlyValues
                        .map(([year, vals]) => ({
                            year,
                            total: (vals.total / vals.students) * 100
                        }))
                        .sort((a, b) => a.year - b.year)
                }))
                .sort((a, b) => d3.ascending(a.instnm, b.instnm));

                // ===== Humanities at UChicago =====
                const humByYear = humanities
                    .filter(d => d.classification === "Humanities and Arts")
                    .map(d => ({
                        year: d.year,
                        total: d.share_students * 100
                    }))
                    .sort((a, b) => a.year - b.year);

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

            setChartData({
                uchicagoByYear: uchicagoByYear,
                ivyPlusByYear: ivyPlusByYear,
                humByYear: humByYear
                });
        });

    }, []);
    
    return (
        <div ref={containerRef} style={{ height: '750vh' }}>
            <div className="flex flex-col lg:flex-row">

                {/* Chart: stuck at top on mobile, side-pinned on desktop */}
                <div className="order-first lg:order-last w-full lg:w-1/2 sticky top-0 h-[55vh] lg:h-screen flex items-center justify-center bg-white z-10">
                    {chartData && <LineChart data={chartData} xKey="year" yKey="total" progress={progresses} />}
                </div>

                {/* Text: below chart on mobile, left side on desktop */}
                <div className="order-last lg:order-first w-full lg:w-1/2 flex flex-col">
                    <div className="h-[10vh] lg:h-[50vh]" />
                    <TextStep v={[0.08, 0.30]} scrollY={scrollYProgress}>
                        <p className="text-lg lg:text-xl">
                            In twenty years, the economics major at Chicago has doubled in size...
                        </p>
                    </TextStep>
                    <TextStep v={[0.32, 0.48]} scrollY={scrollYProgress}>
                        <p className="text-lg lg:text-xl">
                            ...even though it has barely grown at peer institutions.
                        </p>
                    </TextStep>
                    <TextStep v={[0.46, 0.62]} scrollY={scrollYProgress}>
                        <p className="text-lg lg:text-xl">
                            The primary driver of this growth? Business economics.
                        </p>
                    </TextStep>
                    <TextStep v={[0.62, 0.92]} scrollY={scrollYProgress}>
                        <p className="text-lg lg:text-xl">
                            All the while, the humanities and arts have been steadily declining.
                        </p>
                    </TextStep>
                    <div className="h-[80vh]" />
                </div>
            </div>
        </div>
    );
}

// Chart of english and political science
export const AriChartDemo = () => {
    
    const [chartData, setChartData] = useState(null);
    const containerRef = useRef(null);



    useEffect(() => {
        Promise.all([
            d3.csv("data/english_polisci_trend.csv", d3.autoType),
        ]).then(([englishPolisciTrend]) => {
           
            const englishPolisciTrendData = englishPolisciTrend.map(d => ({
                year: d.year,
                english: d["English"] * 100,
                politicalScience: d["Political Science"] * 100
            }));

            setChartData(englishPolisciTrendData);
        });

    }, []);

    return (
        <div ref={containerRef}>
            <AriChart data={chartData} />
        </div>
    )
}

// chart for substitution
export const Substitution = () => {

    const [chartData, setChartData] = useState(null);
    const containerRef = useRef(null);

    useEffect(() => {
        d3.csv("data/substitution_trend.csv", d3.autoType).then((rows) => {
           const byYear = d3.rollup(
            rows,
            v => {
                const entry = { year: v[0].year };
                v.forEach(d => {
                    if (d.classification === "Math and Stats") entry.math = d.share_students * 100;
                    if (d.classification === "Public Policy") entry.publicPolicy = d.share_students * 100;
                });
                return entry;
            },
            d => d.year
        );

        const wideData = Array.from(byYear.values()).sort((a, b) => a.year - b.year);
        setChartData(wideData);
        });
    }, []);

    return (
        <div ref={containerRef} >
            <SubstitutionChart data={chartData} />
        </div>  
    );
};

// chart for penn comparison
export const Penn = () => {

    const [chartData, setChartData] = useState(null);
    const containerRef = useRef(null);

    useEffect(() => {
        d3.csv("data/wharton_uchicago.csv", d3.autoType).then((rows) => {
           const byYear = d3.rollup(
            rows,
            v => {
                const entry = { year: v[0].year };
                v.forEach(d => {
                    if (d.instnm === "University of Chicago") entry.uchicago = d.share_students * 100;
                    if (d.instnm === "University of Pennsylvania") entry.penn = d.share_students * 100;
                });
                return entry;
            },
            d => d.year
        );

        const wideData = Array.from(byYear.values()).sort((a, b) => a.year - b.year);
        setChartData(wideData);
        });
    }, []);

    return (
        <div ref={containerRef} >
            <PennChart data={chartData} />
        </div>  
    );
};

// chart for outcomes
export const Outcomes = () => {

    const [chartData, setChartData] = useState(null);
    const containerRef = useRef(null);

    useEffect(() => {
    d3.csv("data/finance_business.csv", d3.autoType).then((rows) => {
        const wideData = rows
            .map(d => {
                const year = d.year === "2021-2022" ? 2021.5 : +d.year;
                // share is already a percentage, don't multiply
                const share = typeof d.share === 'number' ? d.share : null;
                return { year, share, originalYear: d.year };
            })
            .filter(d => d.share !== null && !isNaN(d.year))
            .sort((a, b) => a.year - b.year);

        console.log('Parsed data:', wideData);
        setChartData(wideData);
    });
}, []);

    return (
        <div ref={containerRef}>
            <OutcomesChart data={chartData} />
        </div>
    );
};

// Two-panel chart for share of students vs. share of degrees
export const SocialSciencesHumanities = () => {

    const [studentsData, setStudentsData] = useState(null);
    const [degreesData, setDegreesData] = useState(null);

    useEffect(() => {
        d3.csv("data/share_social_sciences_humanities.csv", d3.autoType).then((rows) => {
            // Helper that pivots a filtered subset (one metric) into wide format
            const pivotByYear = (filteredRows) => {
                const byYear = d3.rollup(
                    filteredRows,
                    v => {
                        const entry = { year: v[0].year };
                        v.forEach(d => {
                            if (d.is_uchicago === "University of Chicago") entry.uchicago = d.value * 100;
                            if (d.is_uchicago === "Other Ivy Plus") entry.otherIvy = d.value * 100;
                        });
                        return entry;
                    },
                    d => d.year
                );
                return Array.from(byYear.values()).sort((a, b) => a.year - b.year);
            };

            const studentsRows = rows.filter(d => d.metric === "Share of students");
            const degreesRows = rows.filter(d => d.metric === "Share of degrees");

            setStudentsData(pivotByYear(studentsRows));
            setDegreesData(pivotByYear(degreesRows));
        });
    }, []);

   return (
        <div className="my-16">
            <h2 className="text-2xl font-bold text-center mb-6 font-serif">
                Humanities, Arts, and Social Science Majors at UChicago
            </h2>
            <div className="flex flex-col lg:flex-row justify-center items-center gap-6 lg:gap-3">
                <SocialHumChart data={studentsData} title="Share of Students" showLabels={true} />
                <SocialHumChart data={degreesData} title="Share of Degrees" showLabels={true} />
            </div>
        </div>
    );
};


export const MajorExplorer = () => {
    const [allData, setAllData] = useState(null);
    const [majorList, setMajorList] = useState([]);
    const [query, setQuery] = useState("");
    const [selectedMajors, setSelectedMajors] = useState([]);
    const [showDropdown, setShowDropdown] = useState(false);

    // Load data once
    useEffect(() => {
        d3.csv("data/major_shares.csv", d3.autoType).then(rows => {
            setAllData(rows);
            const uniqueMajors = [...new Set(rows.map(d => d.major))].sort();
            setMajorList(uniqueMajors);
        });
    }, []);

    // Filter majors: match query AND not already selected
    const filteredMajors = query.length > 0
        ? majorList
            .filter(m =>
                m.toLowerCase().includes(query.toLowerCase()) &&
                !selectedMajors.includes(m)
            )
            .slice(0, 8)
        : [];

    // Build one series per selected major
    const chartSeries = selectedMajors.map(major => ({
        major,
        values: (allData || [])
            .filter(d => d.major === major)
            .map(d => ({ year: d.year, value: d.share_students * 100 }))
            .sort((a, b) => a.year - b.year)
    }));

    const handleSelect = (major) => {
        setSelectedMajors([...selectedMajors, major]);
        setQuery("");
        setShowDropdown(false);
    };

    const handleRemove = (major) => {
        setSelectedMajors(selectedMajors.filter(m => m !== major));
    };

    const handleInputChange = (e) => {
        setQuery(e.target.value);
        setShowDropdown(true);
    };

    return (
        <div className="my-16 flex flex-col items-center max-w-3xl mx-auto px-4">

            {/* Chips for selected majors */}
            {selectedMajors.length > 0 && (
                <div className="flex flex-wrap gap-2 mb-4 justify-center max-w-2xl">
                    {selectedMajors.map((major, i) => (
                        <span
                            key={major}
                            className="inline-flex items-center bg-gray-100 border border-gray-300 rounded-full px-3 py-1 text-sm font-serif"
                            style={{ borderLeftColor: colorForIndex(i), borderLeftWidth: '4px' }}
                        >
                            {major}
                            <button
                                onClick={() => handleRemove(major)}
                                className="ml-2 text-gray-500 hover:text-gray-900 font-bold"
                                aria-label={`Remove ${major}`}
                            >
                                ×
                            </button>
                        </span>
                    ))}
                </div>
            )}

            {/* Search input + dropdown */}
            <div className="relative w-72 mb-8">
                <input
                    type="text"
                    placeholder={selectedMajors.length === 0 ? "Type a major..." : "Add another major..."}
                    value={query}
                    onChange={handleInputChange}
                    onFocus={() => setShowDropdown(true)}
                    onBlur={() => setTimeout(() => setShowDropdown(false), 150)}
                    className="w-full border border-gray-400 rounded px-3 py-2 text-base font-serif focus:outline-none focus:border-gray-700"
                />

                {showDropdown && filteredMajors.length > 0 && (
                    <ul className="absolute top-full left-0 right-0 mt-1 bg-white border border-gray-300 rounded shadow-lg max-h-60 overflow-y-auto z-10">
                        {filteredMajors.map((major) => (
                            <li
                                key={major}
                                onClick={() => handleSelect(major)}
                                className="px-3 py-2 cursor-pointer hover:bg-gray-100 text-base font-serif"
                            >
                                {major}
                            </li>
                        ))}
                    </ul>
                )}
            </div>

            {/* Chart */}
           {chartSeries.length > 0 ? (
            <figure className="w-full flex flex-col items-center">
                <MajorChart series={chartSeries} />
                <figcaption className="text-sm text-gray-600 italic mt-4 max-w-xl text-left px-4">
                    Share of UChicago graduates majoring in each selected field, 2005–2024. Use the search above to add or remove majors. UChicago-specific majors, such as LLSO, are classified from CIP codes based on best available match.
                </figcaption>
            </figure>
        ) : (
            <p className="text-gray-500 italic font-serif">
                Select one or more majors to see the trend.
            </p>
        )}
        </div>
    );
};

// Helper: assign a color to each selected major by index
const COLORS = ['#800', '#1e3a5f', '#2a7f3e', '#c45a00', '#5e35b1', '#00838f', '#d81b60', '#5d4037'];
function colorForIndex(i) {
    return COLORS[i % COLORS.length];
}