import React, { useState, useEffect } from 'react';
import {  ScrollContainer, IntroAnimation, AriChartDemo, Substitution, Penn, Outcomes, SocialSciencesHumanities, MajorExplorer} from "./Interactives.jsx"
import {
    headlinelg, subhead, byline,
    section1,
    section2, section2Header,
    section3, section3Header,
    section4, section4Header,
    section5, section5Header,
    section6, section6Header,
    editorsNote,

    methodologyHeader,
    methodologyIntro,
    methodologyDataHeader, methodologyDataSubhead, methodologyData,
    methodologySampleHeader, methodologySampleSubhead, methodologySample,
    methodologyClassificationHeader, methodologyClassificationSubhead, methodologyClassification,
    methodologyMeasurementHeader, methodologyMeasurementSubhead, methodologyMeasurement
} from '../../public/content.js';

const ArticleSection = ({ header, paragraphs }) => (
    <section className="max-w-2xl mx-auto px-4 my-12">
        {header && (
            <h2 className="text-3xl font-light mb-6 font-serif">
                {header}
            </h2>
        )}
        {paragraphs.map((paragraph, i) => (
            <p
                className="text-lg leading-relaxed mb-5 font-serif font-normal"
                dangerouslySetInnerHTML={{ __html: paragraph }}
            />
        ))}
    </section>
);

export default function ArticleContent({ windowHeight, windowWidth }) {
    const spacing = .5;
    const height = windowHeight * spacing;

    const [scrollY, setScrollY] = useState(() => {
        const saved = localStorage.getItem('scrollY');
        return saved !== null ? parseInt(saved) : 0;
    });

    const [currentStepIndex, setCurrentStepIndex] = useState(() => {
        const saved = localStorage.getItem('currentStepIndex');
        return saved !== null && scrollY > 2000 ? parseInt(saved) : 0;
    });

    useEffect(() => {
        localStorage.setItem('currentStepIndex', currentStepIndex.toString());
    }, [currentStepIndex]);

    useEffect(() => {
        localStorage.setItem('scrollY', scrollY.toString());
    }, [scrollY]);

    useEffect(() => {
        const handleScroll = () => {
            setScrollY(window.scrollY);
        };
        window.addEventListener('scroll', handleScroll);
        return () => window.removeEventListener('scroll', handleScroll);
    }, []);


    useEffect(() => {
        if (scrollY <= 100) {
            setCurrentStepIndex(0);
        }
    }, [scrollY]);

    const onStepEnter = ({ data }) => {
        setCurrentStepIndex(data);
    };

    const onStepExit = ({ data, direction }) => {
        if (direction === 'up') {
            setCurrentStepIndex(data - 1);
        } else if (direction === 'down') {
            setCurrentStepIndex(data);
        }
    };

    const baseProps = {
        currentStepIndex,
        windowWidth,
        onStepEnter,
        onStepExit,
        height,
    };

    return (
        <div className="[overflow-x:clip]">


            {/* Scrollytelling chart opener */}
            <IntroAnimation />

            {/* Section 1: opening prose */}
            <ArticleSection paragraphs={section1.slice(0, 7)} />

            {/* Penn comparison chart (mentioned at end of section 1) */}
           <figure className="my-16 flex flex-col items-center max-w-3xl mx-auto px-8">
                <div className="w-full">
                    <Penn />
                </div>
                <figcaption className="text-sm text-gray-600 italic mt-4 max-w-xl text-left">
                    The economics major at the University of Chicago used to be half the size of the combination of economics, business, and finance at the University of Pennsylvania. Now, they are on equal footing.
                </figcaption>
            </figure>

             <ArticleSection paragraphs={section1.slice(7, 12)} />

             <figure className="my-16 flex flex-col items-start max-w-2xl mx-auto">
                <img
                    src="photos/ruby_photo.jpg"
                    alt="Vincent Li"
                    className="w-full h-auto"
                />
                <figcaption className="text-sm text-gray-600 italic mt-4 px-4">
                    Ruby Velez, a fourth-year Human Rights major, says that students are thinking: "How do we leverage our education to be the most useful?" <em>Photo by Olin Nafziger.</em>
                </figcaption>
            </figure>

             <ArticleSection paragraphs={section1.slice(12)} />

            {/* Section 2: fiefdom in Saieh */}
            <ArticleSection  paragraphs={section2.slice(0,6)} />

            <figure className="my-16 flex flex-col items-start max-w-2xl mx-auto">
                <img
                    src="photos/vincent_photo.jpg"
                    alt="Vincent Li"
                    className="w-full h-auto"
                />
                <figcaption className="text-sm text-gray-600 italic mt-4 px-4">
                    Vincent Li, a fourth-year buisness economics and LLSO major, said that if he didn't have an "irrational fear" of it, he might have done standard-track economics. <em>Photo by Olin Nafziger.</em>
                </figcaption>
            </figure>

            <ArticleSection  paragraphs={section2.slice(6,9)} />

            {/* Substitution chart (math vs public policy) */}
            <figure className="my-16 flex flex-col items-center max-w-3xl mx-auto px-8">
                <div className="w-full">
                    <Substitution />
                </div>
                <figcaption className="text-sm text-gray-600 italic mt-4 max-w-xl text-left px-4">
                    The share of students majoring in mathematics and statistics, as well as those majoring in public policy has decreased precipitously since 2018. This comes after these majors were steadily growing for the previous 15 years. 
                </figcaption>
            </figure>

            <ArticleSection paragraphs={section2.slice(9, 17)} />

            {/* Outcomes chart (finance/business) */}
            <figure className="my-16 flex flex-col items-center max-w-3xl mx-auto px-4">
                <div className="w-full">
                    <Outcomes />
                </div>
                <figcaption className="text-sm text-gray-600 italic mt-16 max-w-xl text-left px-4">
                    After growing in the early 2010s, the share of students employed in finance or business related jobs after graduating the University of Chicago has not changed since 2017. The number of students majoring in economics nearly doubled during this time. Dashed lines indicate years where no data was available.
                </figcaption>
            </figure>

            <ArticleSection paragraphs={section2.slice(17)} />

                <MajorExplorer />

            {/* Section 3: history of UChicago */}
            <ArticleSection paragraphs={section3.slice(0, 3)} />

            <figure className="my-16 flex flex-col items-start max-w-2xl mx-auto">
                <img
                    src="photos/old_class.jpg"
                    alt="Old class"
                    className="w-full h-auto"
                />
                <figcaption className="text-sm text-gray-600 italic mt-4 px-4">
                      <em>Photo from Chicago Maroon Archive.</em>
                </figcaption>
            </figure>

            <ArticleSection paragraphs={section3.slice(3, 6)} />

             <figure className="my-16 flex flex-col items-start max-w-2xl mx-auto">
                <img
                    src="photos/students_studying.jpg"
                    alt="Old class"
                    className="w-full h-auto"
                />
                <figcaption className="text-sm text-gray-600 italic mt-4 px-4">
                      <em>Photo from Chicago Maroon Archive.</em>
                </figcaption>
            </figure>

            <ArticleSection paragraphs={section3.slice(6)} />

            {/* Section 4: Newman & humanities decline */}
            <ArticleSection paragraphs={section4.slice(0,2)} />

            {/* English/PoliSci chart */}
            <figure className="my-16 flex flex-col items-center max-w-3xl mx-auto px-8">
                 <div className="w-full">
                    <AriChartDemo />
                </div>
                <figcaption className="text-sm text-gray-600 italic mt-4 max-w-xl text-left px-4">
                    In 2005, English and Political Science accounted for 20 percent of graduating students. Today, they make up just over 7 percent of the graduating class. Their decline began around 2012, the year that students entering college during the Great Recession would have graduated. 
                </figcaption>
            </figure>

            <ArticleSection paragraphs={section4.slice(2, 8)} />

            {/* Share of students vs share of degrees chart */}
            <figure className="my-4 flex flex-col items-center max-w-6xl mx-auto px-8">
                <div className="w-full">
                    <SocialSciencesHumanities />
                </div>
                <figcaption className="text-sm text-gray-600 italic mt-0 max-w-xl text-left px-4">
                     Using the share of students, humanities, social sciences, and arts majors at the University of Chicago have declined at the same rate as peer institutions. When looking at the share of degrees, Chicago is declining more quickly, particularly over the past six years.  
                </figcaption>
            </figure>

            <ArticleSection paragraphs={section4.slice(8)} />

            {/* Section 5: humanities response */}
            <ArticleSection paragraphs={section5.slice(0, 4)} />

            <figure className="my-8 flex flex-col items-start max-w-2xl mx-auto">
                <img
                    src="photos/poster_image.jpg"
                    alt="Poster"
                    className="w-full h-auto"
                />
                <figcaption className="text-sm text-gray-600 italic mt-4 px-4">
                    Classes in the humanities, arts, and social sciences frequently advertise themselves with poster displayed around campus. <em>Photo by Olin Nafziger.</em>
                </figcaption>
            </figure>

            <ArticleSection paragraphs={section5.slice(4, 12)} />

            <figure className="my-8 flex flex-col items-start max-w-2xl mx-auto">
                <img
                    src="photos/classroom_image.jpg"
                    alt="Vincent Li"
                    className="w-full h-auto"
                />
                <figcaption className="text-sm text-gray-600 italic mt-4 px-4">
                    Since 2005, the share of students majoring in the humanities, arts, or social sciences has been declining.  <em>Photo by Olin Nafziger.</em>
                </figcaption>
            </figure>

            <ArticleSection paragraphs={section5.slice(12)} />

            {/* Section 6: conclusion */}
            <ArticleSection paragraphs={section6} />

            {/* Editor's note */}
            <section className="max-w-2xl mx-auto px-4 my-12 text-sm">
                <p dangerouslySetInnerHTML={{ __html: editorsNote }} />
            </section>

            {/* Methodology */}
            <section className="max-w-2xl mx-auto px-4 my-16 border-t pt-12">
                <h2 className="text-3xl font-bold mb-6 font-serif">
                    {methodologyHeader}
                </h2>
                {methodologyIntro.map((p, i) => (
                    <p key={i} className="text-lg leading-relaxed mb-5 font-serif"
                    dangerouslySetInnerHTML={{ __html: p }} />
                ))}

                <h3 className="text-2xl font-bold mt-10 mb-2 font-serif">
                    {methodologyDataHeader}
                </h3>
                <p className="text-base italic mb-5 text-gray-700 font-serif">
                    {methodologyDataSubhead}
                </p>
                {methodologyData.map((p, i) => (
                    <p key={i} className="text-lg leading-relaxed mb-5 font-serif"
                    dangerouslySetInnerHTML={{ __html: p }} />
                ))}

                <h3 className="text-2xl font-bold mt-10 mb-2 font-serif">
                    {methodologySampleHeader}
                </h3>
                <p className="text-base italic mb-5 text-gray-700 font-serif">
                    {methodologySampleSubhead}
                </p>
                {methodologySample.map((p, i) => (
                    <p key={i} className="text-lg leading-relaxed mb-5 font-serif"
                    dangerouslySetInnerHTML={{ __html: p }} />
                ))}

                <h3 className="text-2xl font-bold mt-10 mb-2 font-serif">
                    {methodologyClassificationHeader}
                </h3>
                <p className="text-base italic mb-5 text-gray-700 font-serif">
                    {methodologyClassificationSubhead}
                </p>
                {methodologyClassification.map((p, i) => (
                    <p key={i} className="text-lg leading-relaxed mb-5 font-serif"
                    dangerouslySetInnerHTML={{ __html: p }} />
                ))}

                <h3 className="text-2xl font-bold mt-10 mb-2 font-serif">
                    {methodologyMeasurementHeader}
                </h3>
                <p className="text-base italic mb-5 text-gray-700 font-serif">
                    {methodologyMeasurementSubhead}
                </p>
                {methodologyMeasurement.map((p, i) => (
                    <p key={i} className="text-lg leading-relaxed mb-5 font-serif"
                    dangerouslySetInnerHTML={{ __html: p }} />
                ))}
            </section>
        </div>
    );
}
